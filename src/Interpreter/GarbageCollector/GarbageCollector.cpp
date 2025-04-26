#include "GarbageCollector.h"

#include <algorithm>
#include <execution>
#include <ranges>

#ifdef DEBUG
#include "Common\Printer\Printer.h"

#include <chrono>
#include <format>
#endif

bool GarbageCollector::Contains(MidoriTraceable* ptr) const
{
	return m_traceables.contains(ptr);
}

size_t GarbageCollector::GetTotalAllocatedBytes() const
{
	return m_total_bytes_allocated;
}

void GarbageCollector::Mark(GarbageCollector::GarbageCollectionRoots&& roots)
{
#ifdef DEBUG
	std::ranges::for_each
	(
		m_traceables,
		[this](MidoriTraceable* ptr){ Printer::Print<Printer::Color::CYAN>(std::format("Tracked traceable pointer: {:p}, value: {}\n", static_cast<void*>(ptr), ptr->ToText().GetCString())); }
	);
#endif

	// Process each root concurrently.
	std::for_each
	(
		std::execution::unseq,
		roots.begin(), 
		roots.end(),
		[this](MidoriTraceable* ptr){ Trace(ptr); }
	);
}

void GarbageCollector::Sweep()
{
	for (GarbageCollectionRoots::const_iterator it = m_traceables.begin(); it != m_traceables.end(); )
	{
		MidoriTraceable* ptr = *it;
		if (ptr->IsMarked())
		{
			ptr->Unmark();
			++it;
		}
		else
		{
#ifdef DEBUG
			Printer::Print<Printer::Color::RED>(std::format("Deleting traceable pointer: {:p}\n", static_cast<void*>(ptr)));
#endif
			m_total_bytes_allocated -= ptr->GetSize();
			delete ptr;
			it = m_traceables.erase(it);
		}
	}
}

void GarbageCollector::Trace(MidoriTraceable* ptr)
{
	if (ptr->IsMarked())
	{
		return;
	}
#ifdef DEBUG
	Printer::Print<Printer::Color::GREEN>(std::format("Marking traceable pointer: {:p}, value: {}\n", static_cast<void*>(ptr), ptr->ToText().GetCString()));
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
		MidoriArray& cell_values = ptr->GetTraceable<MidoriClosure>().m_cell_values;
		for (int i = 0; i < cell_values.GetLength(); i += 1)
		{
			MidoriValue& value = cell_values[i];
			Trace(value.GetPointer());
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
		MidoriArray& arr = ptr->GetTraceable<MidoriStruct>().m_values;
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
		MidoriArray& arr = ptr->GetTraceable<MidoriUnion>().m_values;

		for (int idx : std::views::iota(0, arr.GetLength()))
		{
			MidoriValue& value = arr[idx];
			if (m_traceables.contains(value.GetPointer()))
			{
				Trace(value.GetPointer());
			}
		}
	}
	else if (ptr->IsTraceable<MidoriBox>())
	{
		// Box value only contains primitive data
	}
}

void GarbageCollector::ReclaimMemory(GarbageCollectionRoots&& roots, bool force_clean)
{
	if (m_total_bytes_allocated < GARBAGE_COLLECTION_THRESHOLD && !force_clean)
	{
		return;
	}

#ifdef DEBUG
	Printer::Print<Printer::Color::BLUE>("\nBefore garbage collection:");
	PrintMemoryTelemetry();
	using Clock = std::chrono::high_resolution_clock;
	using TimePoint = Clock::time_point;
	TimePoint t0 = Clock::now();
#endif

	// MARK
	size_t mark_count = 0;
#ifdef DEBUG
	TimePoint t_mark_start = Clock::now();
#endif
	std::ranges::for_each(roots, [this](MidoriTraceable* root) { Trace(root); });
#ifdef DEBUG
	TimePoint t_mark_end = Clock::now();
#endif

	// SWEEP
	size_t sweep_count = 0u;
	size_t bytes_reclaimed = 0u;
#ifdef DEBUG
	TimePoint t_sweep_start = Clock::now();
#endif
	for (GarbageCollectionRoots::iterator it = m_traceables.begin(); it != m_traceables.end(); )
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
			delete ptr;
			it = m_traceables.erase(it);
		}
	}
#ifdef DEBUG
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
}

void GarbageCollector::CleanUp()
{
	ReclaimMemory({}, true);
}

#ifdef DEBUG
void GarbageCollector::PrintMemoryTelemetry()
{
	Printer::Print<Printer::Color::BLUE>
		(
			std::format
			(
				"\n\t------------------------------\n"
				"\tMemory telemetry:\n"
				"\tHeap pointers allocated: {}\n"
				"\tTotal Bytes allocated: {}\n"
				"\t------------------------------\n\n",
				m_traceables.size(),
				m_total_bytes_allocated
			)
		);
}
#endif