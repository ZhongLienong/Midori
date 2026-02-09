#include "MidoriAllocator.h"

#include <algorithm>
#include <cstdlib>
#include <optional>
#include <utility>

#ifdef __EMSCRIPTEN__

MidoriAllocator::MidoriAllocator() = default;
MidoriAllocator::~MidoriAllocator() = default;

void* MidoriAllocator::Allocate(size_t size)
{
	if (size == 0uz)
	{
		return nullptr;
	}

	void* ptr = std::malloc(size);
	if (ptr != nullptr)
	{
		m_allocated.emplace(ptr);
	}
	return ptr;
}

MidoriAllocator& MidoriAllocator::Free(void* ptr, size_t)
{
	if (ptr != nullptr)
	{
		m_allocated.erase(ptr);
	}
	std::free(ptr);
	return *this;
}

MidoriAllocator&& MidoriAllocator::Free(void* ptr, size_t size) &&
{
	static_cast<MidoriAllocator&>(*this).Free(ptr, size);
	return std::move(*this);
}

bool MidoriAllocator::Contains(const void* ptr) const noexcept
{
	if (ptr == nullptr)
	{
		return false;
	}

	return m_allocated.contains(const_cast<void*>(ptr));
}

#else

namespace
{
	struct SlotBit
	{
		size_t m_word_index = 0uz;
		uint64_t m_mask = 0ull;
	};

	std::optional<SlotBit> TryComputeSlotBit(const uint8_t* base, const void* ptr) noexcept
	{
		if (base == nullptr || ptr == nullptr)
		{
			return std::nullopt;
		}

		const uintptr_t base_addr = reinterpret_cast<uintptr_t>(base);
		const uintptr_t addr = reinterpret_cast<uintptr_t>(ptr);
		if (addr < base_addr)
		{
			return std::nullopt;
		}

		const size_t offset = static_cast<size_t>(addr - base_addr);
		if (offset >= MidoriAllocator::BLOCK_SIZE)
		{
			return std::nullopt;
		}

		if (offset % MidoriAllocator::SLOT_SIZE != 0uz)
		{
			return std::nullopt;
		}

		const size_t slot_index = offset / MidoriAllocator::SLOT_SIZE;
		if (slot_index >= MidoriAllocator::SLOTS_PER_BLOCK)
		{
			return std::nullopt;
		}

		const size_t word_index = slot_index / 64uz;
		const uint64_t mask = 1ull << (slot_index % 64uz);

		return SlotBit{word_index, mask};
	}
}

MidoriAllocator::MidoriAllocator()
{
	AllocateBlock();
}

MidoriAllocator::~MidoriAllocator()
{
	for (const BlockInfo& block : m_blocks)
	{
		std::free(block.m_base);
	}
	m_blocks.clear();
	m_large_allocs.clear();
	m_free_list = nullptr;
}

void* MidoriAllocator::Allocate(size_t size)
{
	if (size == 0uz)
	{
		return nullptr;
	}

	if (size > SLOT_SIZE)
	{
		return AllocateLarge(size);
	}

	return AllocateSmall();
}

void* MidoriAllocator::AllocateSmall()
{
	if (!EnsureFreeList())
	{
		return nullptr;
	}

	FreeNode* node = PopFreeNode();
	if (node == nullptr)
	{
		return nullptr;
	}

	if (!SetLiveBit(node, true))
	{
		PushFreeNode(node);
		return nullptr;
	}
	return static_cast<void*>(node);
}

void* MidoriAllocator::AllocateLarge(size_t size)
{
	void* ptr = std::malloc(size);
	if (ptr == nullptr)
	{
		return nullptr;
	}

	if (!TrackLargeAllocation(ptr))
	{
		std::free(ptr);
		return nullptr;
	}
	return ptr;
}

MidoriAllocator& MidoriAllocator::Free(void* ptr, size_t size) &
{
	if (ptr == nullptr || size == 0uz)
	{
		return *this;
	}

	if (size <= SLOT_SIZE)
	{
		if (SetLiveBit(ptr, false))
		{
			PushFreeNode(static_cast<FreeNode*>(ptr));
		}
		return *this;
	}

	UntrackLargeAllocation(ptr);
	std::free(ptr);
	return *this;
}

MidoriAllocator&& MidoriAllocator::Free(void* ptr, size_t size) &&
{
	static_cast<MidoriAllocator&>(*this).Free(ptr, size);
	return std::move(*this);
}

bool MidoriAllocator::Contains(const void* ptr) const noexcept
{
	if (ptr == nullptr)
	{
		return false;
	}

	const BlockInfo* block = FindBlock(ptr);
	if (block != nullptr)
	{
		std::optional<SlotBit> slot = TryComputeSlotBit(block->m_base, ptr);
		if (slot.has_value())
		{
			const uint64_t live_word = block->m_live[slot->m_word_index];
			return (live_word & slot->m_mask) != 0ull;
		}
	}

	return ContainsLargeAllocation(ptr);
}

bool MidoriAllocator::AllocateBlock()
{
	void* block = std::malloc(BLOCK_SIZE);
	if (block == nullptr)
	{
		return false;
	}

	BlockInfo info;
	info.m_base = static_cast<uint8_t*>(block);
	const uintptr_t base_addr = reinterpret_cast<uintptr_t>(info.m_base);
	std::vector<BlockInfo>::iterator insert_it = std::upper_bound
	(
		m_blocks.begin(),
		m_blocks.end(),
		base_addr,
		[](uintptr_t value, const BlockInfo& entry)
		{
			return value < reinterpret_cast<uintptr_t>(entry.m_base);
		}
	);
	m_blocks.insert(insert_it, info);

	uint8_t* slot_ptr = info.m_base;
	for (size_t i = 0uz; i < SLOTS_PER_BLOCK; i += 1uz)
	{
		FreeNode* node = reinterpret_cast<FreeNode*>(slot_ptr);
		PushFreeNode(node);
		slot_ptr += SLOT_SIZE;
	}

	return true;
}

bool MidoriAllocator::EnsureFreeList()
{
	if (m_free_list != nullptr)
	{
		return true;
	}

	return AllocateBlock();
}

MidoriAllocator::FreeNode* MidoriAllocator::PopFreeNode() noexcept
{
	if (m_free_list == nullptr)
	{
		return nullptr;
	}

	FreeNode* node = m_free_list;
	m_free_list = node->m_next;
	return node;
}

MidoriAllocator::FreeNode* MidoriAllocator::PushFreeNode(FreeNode* node) noexcept
{
	if (node == nullptr)
	{
		return m_free_list;
	}

	node->m_next = m_free_list;
	m_free_list = node;
	return node;
}

MidoriAllocator::BlockInfo* MidoriAllocator::FindBlock(const void* ptr) noexcept
{
	return const_cast<BlockInfo*>(static_cast<const MidoriAllocator*>(this)->FindBlock(ptr));
}

const MidoriAllocator::BlockInfo* MidoriAllocator::FindBlock(const void* ptr) const noexcept
{
	if (m_blocks.empty() || ptr == nullptr)
	{
		return nullptr;
	}

	const uintptr_t addr = reinterpret_cast<uintptr_t>(ptr);
	std::vector<BlockInfo>::const_iterator it = std::upper_bound
	(
		m_blocks.begin(),
		m_blocks.end(),
		addr,
		[](uintptr_t value, const BlockInfo& entry)
		{
			return value < reinterpret_cast<uintptr_t>(entry.m_base);
		}
	);

	if (it == m_blocks.begin())
	{
		return nullptr;
	}

	--it;
	return &(*it);
}

bool MidoriAllocator::SetLiveBit(void* ptr, bool is_live) noexcept
{
	BlockInfo* block = FindBlock(ptr);
	if (block == nullptr)
	{
		return false;
	}

	std::optional<SlotBit> slot = TryComputeSlotBit(block->m_base, ptr);
	if (!slot.has_value())
	{
		return false;
	}

	uint64_t& live_word = block->m_live[slot->m_word_index];
	if (is_live)
	{
		live_word |= slot->m_mask;
	}
	else
	{
		live_word &= ~slot->m_mask;
	}
	return true;
}

bool MidoriAllocator::TrackLargeAllocation(void* ptr)
{
	if (ptr == nullptr)
	{
		return false;
	}

	m_large_allocs.push_back(ptr);
	return true;
}

bool MidoriAllocator::UntrackLargeAllocation(void* ptr) noexcept
{
	if (ptr == nullptr || m_large_allocs.empty())
	{
		return false;
	}

	std::vector<void*>::iterator it = std::find(m_large_allocs.begin(), m_large_allocs.end(), ptr);
	if (it == m_large_allocs.end())
	{
		return false;
	}

	*it = m_large_allocs.back();
	m_large_allocs.pop_back();
	return true;
}

bool MidoriAllocator::ContainsLargeAllocation(const void* ptr) const noexcept
{
	if (ptr == nullptr || m_large_allocs.empty())
	{
		return false;
	}

	std::vector<void*>::const_iterator it = std::find_if
	(
		m_large_allocs.begin(),
		m_large_allocs.end(),
		[ptr](const void* entry)
		{
			return entry == ptr;
		}
	);

	return it != m_large_allocs.end();
}

#endif
