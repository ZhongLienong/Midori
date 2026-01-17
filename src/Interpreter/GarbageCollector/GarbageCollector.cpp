#include "GarbageCollector.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Interpreter/Allocator/MidoriAllocator.h"

#include <algorithm>
#include <ranges>

#if MIDORI_DEBUG_INFO
#include "Common\Printer\Printer.h"

#include <chrono>
#include <format>

namespace
{
	std::string FormatTime(int64_t nanoseconds)
	{
		constexpr int64_t NS_PER_US = 1'000;
		constexpr int64_t NS_PER_MS = 1'000'000;
		constexpr int64_t NS_PER_S = 1'000'000'000;

		if (nanoseconds < NS_PER_US)
		{
			return std::format("{} ns", nanoseconds);
		}
		else if (nanoseconds < NS_PER_MS)
		{
			int64_t us = nanoseconds / NS_PER_US;
			int64_t ns = nanoseconds % NS_PER_US;
			if (ns == 0)
			{
				return std::format("{} us", us);
			}
			return std::format("{} us {} ns", us, ns);
		}
		else if (nanoseconds < NS_PER_S)
		{
			int64_t ms = nanoseconds / NS_PER_MS;
			int64_t us = (nanoseconds % NS_PER_MS) / NS_PER_US;
			if (us == 0)
			{
				return std::format("{} ms", ms);
			}
			return std::format("{} ms {} us", ms, us);
		}
		else
		{
			int64_t s = nanoseconds / NS_PER_S;
			int64_t ms = (nanoseconds % NS_PER_S) / NS_PER_MS;
			if (ms == 0)
			{
				return std::format("{} s", s);
			}
			return std::format("{} s {} ms", s, ms);
		}
	}

	std::string FormatBytes(size_t bytes)
	{
		constexpr double KB = 1024.0;
		constexpr double MB = 1024.0 * KB;
		constexpr double GB = 1024.0 * MB;

		if (bytes < 1024)
		{
			return std::format("{} bytes", bytes);
		}
		else if (bytes < 1024 * 1024)
		{
			double kb = static_cast<double>(bytes) / KB;
			return std::format("{:.2f} KB", kb);
		}
		else if (bytes < 1024 * 1024 * 1024)
		{
			double mb = static_cast<double>(bytes) / MB;
			return std::format("{:.2f} MB", mb);
		}
		else
		{
			double gb = static_cast<double>(bytes) / GB;
			return std::format("{:.2f} GB", gb);
		}
	}
}
#endif

bool GarbageCollector::Contains(MidoriTraceable* ptr) const
{
	return m_traceables.contains(ptr);
}

void GarbageCollector::RegisterObject(MidoriTraceable* traceable)
{
	size_t object_size = traceable->GetSize();
	m_total_bytes_allocated += object_size;
	m_traceables.emplace(traceable);
	m_object_sizes[traceable] = object_size;
}

void GarbageCollector::Trace(MidoriTraceable* ptr)
{
	if (ptr->IsMarked())
	{
		return;
	}
#if MIDORI_DEBUG_FULL
	Printer::Print<Printer::Color::GREEN>(std::format("Marking traceable pointer: {:p}\n", static_cast<void*>(ptr)));
#endif
	ptr->Mark();

	if (ptr->IsTraceable<MidoriArray>())
	{
		MidoriArray& arr = ptr->GetTraceable<MidoriArray>();
		for (int idx : std::views::iota(0, arr.GetLength()))
		{
			MidoriValue& value = arr[idx];
			if (m_traceables.contains(value.GetPointer()))
			{
				Trace(value.GetPointer());
			}
		}
	}
	else if (ptr->IsTraceable<MidoriClosure>())
	{
		MidoriTuple& cell_values = ptr->GetTraceable<MidoriClosure>().m_cell_values;
		for (int i = 0; i < cell_values.GetLength(); i += 1)
		{
			MidoriValue& value = cell_values[i];
			if (m_traceables.contains(value.GetPointer()))
			{
				Trace(value.GetPointer());
			}
		}
	}
	else if (ptr->IsTraceable<MidoriCellValue>())
	{
		MidoriValue cell_value = ptr->GetTraceable<MidoriCellValue>().GetValue();
		if (m_traceables.contains(cell_value.GetPointer()))
		{
			Trace(cell_value.GetPointer());
		}
	}
	else if (ptr->IsTraceable<MidoriStruct>())
	{
		MidoriTuple& arr = ptr->GetTraceable<MidoriStruct>().m_values;
		for (int idx : std::views::iota(0, arr.GetLength()))
		{
			MidoriValue& value = arr[idx];
			if (m_traceables.contains(value.GetPointer()))
			{
				Trace(value.GetPointer());
			}
		}
	}
	else if (ptr->IsTraceable<MidoriUnion>())
	{
		MidoriTuple& arr = ptr->GetTraceable<MidoriUnion>().m_values;

		for (int idx : std::views::iota(0, arr.GetLength()))
		{
			MidoriValue& value = arr[idx];
			if (m_traceables.contains(value.GetPointer()))
			{
				Trace(value.GetPointer());
			}
		}
	}
	else if (ptr->IsTraceable<MidoriFuture>())
	{
		MidoriFuture& future = ptr->GetTraceable<MidoriFuture>();
		if (m_traceables.contains(future.m_result.GetPointer()))
		{
			Trace(future.m_result.GetPointer());
		}

		if (future.m_closure)
		{
			MidoriTuple& cell_values = future.m_closure->m_cell_values;
			for (int i = 0; i < cell_values.GetLength(); i += 1)
			{
				MidoriValue& value = cell_values[i];
				if (m_traceables.contains(value.GetPointer()))
				{
					Trace(value.GetPointer());
				}
			}
		}
	}
}

void GarbageCollector::ReclaimMemory(GarbageCollectionRoots&& roots, MidoriAllocator& allocator, bool force_clean)
{
	if (m_total_bytes_allocated < m_gc_threshold && !force_clean)
	{
		return;
	}

#if MIDORI_DEBUG_INFO
	Printer::Print<Printer::Color::BLUE>("\n----------------------------------------------\nBefore garbage collection:");
	PrintMemoryTelemetry();
	using Clock = std::chrono::high_resolution_clock;
	using TimePoint = Clock::time_point;
	TimePoint t0 = Clock::now();
#endif

	size_t mark_count = 0u;
#if MIDORI_DEBUG_INFO
	TimePoint t_mark_start = Clock::now();
#endif
	// Mark
	for (MidoriTraceable* root : roots)
	{
		Trace(root);
	}
#if MIDORI_DEBUG_INFO
	TimePoint t_mark_end = Clock::now();
#endif

	size_t sweep_count = 0u;
	size_t bytes_reclaimed = 0u;
#if MIDORI_DEBUG_INFO
	TimePoint t_sweep_start = Clock::now();
#endif

	for (std::unordered_set<MidoriTraceable*>::iterator it = m_traceables.begin(); it != m_traceables.end(); )
	{
		MidoriTraceable* ptr = *it;
		if (ptr->IsMarked())
		{
			ptr->Unmark();
			++mark_count;
			++it;
		}
		else
		{
			++sweep_count;
			size_t registered_size = m_object_sizes[ptr];
			bytes_reclaimed += registered_size;
			if (m_total_bytes_allocated >= registered_size)
			{
				m_total_bytes_allocated -= registered_size;
			}
			else
			{
				m_total_bytes_allocated = 0uz;
			}
			ptr->~MidoriTraceable();
			allocator.Free(ptr);
			m_object_sizes.erase(ptr);
			it = m_traceables.erase(it);
		}
	}

#if MIDORI_DEBUG_INFO
	TimePoint t_sweep_end = Clock::now();
	TimePoint t1 = Clock::now();

	int64_t ns_mark = std::chrono::duration_cast<std::chrono::nanoseconds>(t_mark_end - t_mark_start).count();
	int64_t ns_sweep = std::chrono::duration_cast<std::chrono::nanoseconds>(t_sweep_end - t_sweep_start).count();
	int64_t ns_total = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count();

	Printer::Print<Printer::Color::BLUE>
		(
			std::format
			(
				"\n[GC] Mark time:    {}\n"
				"[GC] Sweep time:   {}\n"
				"[GC] Total time:   {}\n"
				"[GC] Roots traced: {}\n"
				"[GC] Survivors:    {}\n"
				"[GC] Collected:    {} ({})\n",
				FormatTime(ns_mark),
				FormatTime(ns_sweep),
				FormatTime(ns_total),
				roots.size(),
				mark_count,
				sweep_count,
				FormatBytes(bytes_reclaimed)
			)
		);
	Printer::Print<Printer::Color::BLUE>("\nAfter garbage collection:");
	PrintMemoryTelemetry();
#endif

	size_t new_threshold = static_cast<size_t>(static_cast<double>(m_total_bytes_allocated) * GC_GROWTH_FACTOR);
	new_threshold = std::max(new_threshold, MIN_GC_THRESHOLD);
	new_threshold = std::min(new_threshold, MAX_GC_THRESHOLD);
	m_gc_threshold = new_threshold;
}

bool GarbageCollector::ShouldCollect() const
{
	return m_total_bytes_allocated >= m_gc_threshold;
}

#if MIDORI_DEBUG_INFO
void GarbageCollector::PrintMemoryTelemetry()
{
	Printer::Print<Printer::Color::BLUE>
		(
			std::format
			(
				"Total allocated: {}\nObject count:    {}\n----------------------------------------------\n",
				FormatBytes(m_total_bytes_allocated),
				m_traceables.size()
			)
		);
}
#endif
