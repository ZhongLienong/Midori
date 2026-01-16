#include "GarbageCollector.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Interpreter/Allocator/MidoriAllocator.h"

#include <algorithm>
#include <ranges>

#if MIDORI_DEBUG_INFO
#include "Common\Printer\Printer.h"

#include <chrono>
#include <format>
#endif

bool GarbageCollector::Contains(MidoriTraceable* ptr) const
{
	return m_traceables.contains(ptr);
}

void GarbageCollector::RegisterObject(MidoriTraceable* traceable)
{
	m_total_bytes_allocated += traceable->GetSize();
	m_traceables.emplace(traceable);
}

void GarbageCollector::Trace(MidoriTraceable* ptr)
{
	if (ptr->IsMarked())
	{
		return;
	}
#if MIDORI_DEBUG_INFO
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
	Printer::Print<Printer::Color::BLUE>("\nBefore garbage collection:");
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
			bytes_reclaimed += ptr->GetSize();
			m_total_bytes_allocated -= ptr->GetSize();
			ptr->~MidoriTraceable();
			allocator.Free(ptr);
			it = m_traceables.erase(it);
		}
	}

#if MIDORI_DEBUG_INFO
	TimePoint t_sweep_end = Clock::now();
	TimePoint t1 = Clock::now();

	int64_t ms_mark = std::chrono::duration_cast<std::chrono::milliseconds>(t_mark_end - t_mark_start).count();
	int64_t ms_sweep = std::chrono::duration_cast<std::chrono::milliseconds>(t_sweep_end - t_sweep_start).count();
	int64_t ms_total = std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count();

	Printer::Print<Printer::Color::BLUE>
		(
			std::format
			(
				"\n[GC] Mark time:    {} ms\n"
				"[GC] Sweep time:   {} ms\n"
				"[GC] Total time:   {} ms\n"
				"[GC] Roots traced: {}\n"
				"[GC] Survivors:    {}\n"
				"[GC] Collected:    {} ({} bytes)\n",
				ms_mark,
				ms_sweep,
				ms_total,
				roots.size(),
				mark_count,
				sweep_count,
				bytes_reclaimed
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
				"Total allocated: {} bytes\nObject count:    {}\n",
				m_total_bytes_allocated,
				m_traceables.size()
			)
		);
}
#endif
