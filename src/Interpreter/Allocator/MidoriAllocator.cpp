#include "MidoriAllocator.h"

#include <cstdlib>
#include <new>

#ifdef __EMSCRIPTEN__

MidoriAllocator::MidoriAllocator() = default;
MidoriAllocator::~MidoriAllocator() = default;

void* MidoriAllocator::Allocate(size_t size)
{
	if (size == 0uz)
	{
		return nullptr;
	}
	else
	{
		return std::malloc(size);
	}
}

void MidoriAllocator::Free(void* ptr, size_t)
{
	std::free(ptr);
}

#else

MidoriAllocator::MidoriAllocator()
{
	AllocateBlock();
}

MidoriAllocator::~MidoriAllocator()
{
	for (void* block : m_blocks)
	{
		std::free(block);
	}
	m_blocks.clear();
	m_free_list = nullptr;
}

void MidoriAllocator::AllocateBlock()
{
	void* block = std::malloc(BLOCK_SIZE);
	if (block == nullptr)
	{
		return;
	}
	m_blocks.emplace_back(block);

	uint8_t* slot_ptr = static_cast<uint8_t*>(block);
	for (size_t i = 0uz; i < SLOTS_PER_BLOCK; i += 1uz)
	{
		FreeNode* node = reinterpret_cast<FreeNode*>(slot_ptr);
		node->m_next = m_free_list;
		m_free_list = node;
		slot_ptr += SLOT_SIZE;
	}
}

void* MidoriAllocator::Allocate(size_t size)
{
	if (size == 0uz)
	{
		return nullptr;
	}

	if (size > SLOT_SIZE)
	{
		return std::malloc(size);
	}

	if (m_free_list == nullptr)
	{
		AllocateBlock();
	}

	if (m_free_list == nullptr)
	{
		return nullptr;
	}

	FreeNode* node = m_free_list;
	m_free_list = node->m_next;
	return static_cast<void*>(node);
}

void MidoriAllocator::Free(void* ptr, size_t size)
{
	if (ptr == nullptr || size == 0uz)
	{
		return;
	}

	if (size <= SLOT_SIZE)
	{
		FreeNode* node = static_cast<FreeNode*>(ptr);
		node->m_next = m_free_list;
		m_free_list = node;
	}
	else
	{
		std::free(ptr);
	}
}

#endif
