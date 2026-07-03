#pragma once

#include <cstddef>
#include <cstdint>
#include <unordered_set>
#include <vector>

class MidoriAllocator
{
public:
	static constexpr size_t BLOCK_SIZE = 65536uz;
	static constexpr size_t SLOT_SIZE = 80uz;
	static constexpr size_t SLOTS_PER_BLOCK = BLOCK_SIZE / SLOT_SIZE;

	MidoriAllocator();
	~MidoriAllocator();

	MidoriAllocator(const MidoriAllocator&) = delete;
	MidoriAllocator& operator=(const MidoriAllocator&) = delete;

	void* Allocate(size_t size);
	MidoriAllocator& Free(void* ptr, size_t size) &;
	MidoriAllocator&& Free(void* ptr, size_t size) &&;

	bool Contains(const void* ptr) const noexcept;

private:
#ifndef __EMSCRIPTEN__
	struct FreeNode
	{
		FreeNode* m_next;
	};

	static constexpr size_t RESERVED_REGION_SIZE = 1uz << 30uz;
	static constexpr size_t USABLE_BLOCK_BYTES = SLOTS_PER_BLOCK * SLOT_SIZE;
	static constexpr size_t LIVE_WORDS_PER_BLOCK = (SLOTS_PER_BLOCK + 63uz) / 64uz;

	uint8_t* m_region_base = nullptr;
	size_t m_committed_bytes = 0uz;
	std::vector<uint64_t> m_live_bits;
	FreeNode* m_free_list = nullptr;
	std::vector<void*> m_large_allocs;

	void* AllocateSmall();
	void* AllocateLarge(size_t size);
	bool AllocateBlock();
	bool EnsureFreeList();
	FreeNode* PopFreeNode() noexcept;
	FreeNode* PushFreeNode(FreeNode* node) noexcept;
	bool SetLiveBit(void* ptr, bool is_live) noexcept;
	bool TrackLargeAllocation(void* ptr);
	bool UntrackLargeAllocation(void* ptr) noexcept;
	bool ContainsLargeAllocation(const void* ptr) const noexcept;
#else
	std::unordered_set<void*> m_allocated;
#endif
};
