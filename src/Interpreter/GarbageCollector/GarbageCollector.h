#pragma once

#include "Common/Value/Value.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Interpreter/Allocator/MidoriAllocator.h"

#include <cstddef>
#include <cstdint>
#include <vector>

class GarbageCollector
{
public:
	static constexpr inline size_t INITIAL_GC_THRESHOLD = 512000uz * 8uz;
	static constexpr inline size_t MIN_GC_THRESHOLD = 512000uz * 4uz;
	static constexpr inline size_t MAX_GC_THRESHOLD = 512000uz * 128uz;
	static constexpr inline double GC_GROWTH_FACTOR = 1.5;

	using GarbageCollectionRoots = std::vector<MidoriTraceable*>;

private:
	size_t m_total_bytes_allocated = 0uz;
	size_t m_gc_threshold = INITIAL_GC_THRESHOLD;
	std::vector<uint64_t> m_mark_bits;
	std::vector<MidoriTraceable*> m_mark_stack;
	const MidoriAllocator* m_allocator = nullptr;

public:
	GarbageCollector() = default;
	~GarbageCollector() = default;

	GarbageCollector(const GarbageCollector&) = delete;
	GarbageCollector& operator=(const GarbageCollector&) = delete;

	void ReclaimMemory(const GarbageCollectionRoots& roots, MidoriAllocator& allocator, bool force_clean = false);

	MIDORI_FORCE_INLINE bool ShouldCollect() const noexcept
	{
		return m_total_bytes_allocated >= m_gc_threshold;
	}

	size_t TotalBytesAllocated() const noexcept { return m_total_bytes_allocated; }

	void SetAllocator(const MidoriAllocator* allocator) noexcept { m_allocator = allocator; }

	MIDORI_FORCE_INLINE void RegisterObject(MidoriTraceable* traceable) noexcept
	{
		m_total_bytes_allocated += traceable->GetSize();
	}

#if MIDORI_DEBUG_INFO
	void PrintMemoryTelemetry();
#endif

	bool Contains(MidoriTraceable* ptr) const;

private:
	void Trace(const GarbageCollectionRoots& roots);
	void TryMark(MidoriTraceable* child_ptr);
	void Sweep(MidoriAllocator& allocator, size_t& sweep_count, size_t& bytes_reclaimed);
};
