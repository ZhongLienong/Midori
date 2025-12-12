#pragma once

#include "Common/Value/Value.h"
#include "Common/BuildConfig/BuildConfig.h"

#include <vector>

class GarbageCollector
{
public:
	static constexpr inline size_t GARBAGE_COLLECTION_THRESHOLD = 512000uz * 4uz;

	// Garbage collection utilities
	using GarbageCollectionRoots = std::vector<MidoriTraceable*>;

private:
	size_t m_total_bytes_allocated = 0u;
	std::unordered_set<MidoriTraceable*> m_traceables;

public:
	void ReclaimMemory(GarbageCollectionRoots&& roots, bool force_clean = false);

	bool ShouldCollect() const;

	void CleanUp();

#if MIDORI_DEBUG_INFO
	void PrintMemoryTelemetry();
#endif

	template<typename T>
	MidoriTraceable* AllocateTraceable(T&& arg, PointerTag tag)
	{
		MidoriTraceable* traceable = new MidoriTraceable(std::forward<T>(arg));
		MidoriTaggedPointer tagged_pointer(traceable, tag);
		m_total_bytes_allocated += traceable->GetSize();
		m_traceables.emplace(traceable);

		return static_cast<MidoriTraceable*>(tagged_pointer);
	}

	bool Contains(MidoriTraceable* ptr) const;

	size_t GetTotalAllocatedBytes() const;

private:
	void Mark(GarbageCollectionRoots&& roots);

	void Sweep();

	void Trace(MidoriTraceable* ptr);
};
