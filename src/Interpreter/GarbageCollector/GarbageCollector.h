#pragma once

#include "Common/Value/Value.h"
#include "Common/BuildConfig/BuildConfig.h"

#include <functional>
#include <vector>
#include <unordered_set>

class MidoriAllocator;

class GarbageCollector
{
public:
	static constexpr inline size_t INITIAL_GC_THRESHOLD = 512000uz * 8uz;
	static constexpr inline size_t MIN_GC_THRESHOLD = 512000uz * 4uz;
	static constexpr inline size_t MAX_GC_THRESHOLD = 512000uz * 128uz;
	static constexpr inline double GC_GROWTH_FACTOR = 1.5;

	using GarbageCollectionRoots = std::vector<MidoriTraceable*>;
	using Deallocator = std::function<void(MidoriTraceable*)>;

private:
	size_t m_total_bytes_allocated = 0uz;
	size_t m_gc_threshold = INITIAL_GC_THRESHOLD;
	std::unordered_set<MidoriTraceable*> m_traceables;

public:
	GarbageCollector() = default;
	~GarbageCollector() = default;

	GarbageCollector(const GarbageCollector&) = delete;
	GarbageCollector& operator=(const GarbageCollector&) = delete;

	void ReclaimMemory(GarbageCollectionRoots&& roots, MidoriAllocator& allocator, bool force_clean = false);

	bool ShouldCollect() const;

	void RegisterObject(MidoriTraceable* traceable);

#if MIDORI_DEBUG_INFO
	void PrintMemoryTelemetry();
#endif

	bool Contains(MidoriTraceable* ptr) const;

private:
	void Trace(MidoriTraceable* ptr);
};
