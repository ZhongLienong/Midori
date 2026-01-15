#pragma once

#include <cstddef>
#include <cstdint>
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
	void Free(void* ptr);

private:
#ifndef __EMSCRIPTEN__
	struct FreeNode
	{
		FreeNode* m_next;
	};

	std::vector<void*> m_blocks;
	FreeNode* m_free_list = nullptr;

	void AllocateBlock();
#endif
};
