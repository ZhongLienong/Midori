#pragma once

#include <cstddef>
#include <cstdint>
#include <array>
#include <vector>
#include <unordered_set>

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

	static constexpr size_t s_live_word_count = (SLOTS_PER_BLOCK + 63uz) / 64uz;

	struct BlockInfo
	{
		uint8_t* m_base = nullptr;
		std::array<uint64_t, s_live_word_count> m_live{};
	};

	std::vector<BlockInfo> m_blocks;
	FreeNode* m_free_list = nullptr;
	std::vector<void*> m_large_allocs;

	void* AllocateSmall();
	void* AllocateLarge(size_t size);
	bool AllocateBlock();
	bool EnsureFreeList();
	FreeNode* PopFreeNode() noexcept;
	FreeNode* PushFreeNode(FreeNode* node) noexcept;
	BlockInfo* FindBlock(const void* ptr) noexcept;
	const BlockInfo* FindBlock(const void* ptr) const noexcept;
	bool SetLiveBit(void* ptr, bool is_live) noexcept;
	bool TrackLargeAllocation(void* ptr);
	bool UntrackLargeAllocation(void* ptr) noexcept;
	bool ContainsLargeAllocation(const void* ptr) const noexcept;
#else
	std::unordered_set<void*> m_allocated;
#endif
};
