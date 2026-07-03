#include "MidoriAllocator.h"
#include "Common/Value/Value.h"

#include <algorithm>
#include <cstdlib>

static_assert(sizeof(MidoriTraceable) <= MidoriAllocator::SLOT_SIZE, "MidoriTraceable must fit one allocator slot");

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

MidoriAllocator& MidoriAllocator::Free(void* ptr, size_t size) &
{
	(void)size;
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

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#else
#include <sys/mman.h>

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif

MidoriAllocator::MidoriAllocator()
{
#ifdef _WIN32
	m_region_base = static_cast<uint8_t*>(VirtualAlloc(nullptr, RESERVED_REGION_SIZE, MEM_RESERVE, PAGE_NOACCESS));
#else
	void* region = mmap(nullptr, RESERVED_REGION_SIZE, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
	m_region_base = region == MAP_FAILED ? nullptr : static_cast<uint8_t*>(region);
#endif

	AllocateBlock();
}

MidoriAllocator::~MidoriAllocator()
{
	if (m_region_base != nullptr)
	{
#ifdef _WIN32
		VirtualFree(m_region_base, 0u, MEM_RELEASE);
#else
		static_cast<void>(munmap(m_region_base, RESERVED_REGION_SIZE));
#endif
		m_region_base = nullptr;
	}

	m_committed_bytes = 0uz;
	m_live_bits.clear();
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

bool MidoriAllocator::AllocateBlock()
{
	if (m_region_base == nullptr || m_committed_bytes >= RESERVED_REGION_SIZE)
	{
		return false;
	}

	uint8_t* block_base = m_region_base + m_committed_bytes;
#ifdef _WIN32
	if (VirtualAlloc(block_base, BLOCK_SIZE, MEM_COMMIT, PAGE_READWRITE) == nullptr)
	{
		return false;
	}
#else
	if (mprotect(block_base, BLOCK_SIZE, PROT_READ | PROT_WRITE) != 0)
	{
		return false;
	}
#endif

	m_live_bits.insert(m_live_bits.end(), LIVE_WORDS_PER_BLOCK, 0ull);
	m_committed_bytes += BLOCK_SIZE;

	uint8_t* slot_ptr = block_base;
	for (size_t i = 0uz; i < SLOTS_PER_BLOCK; i += 1uz)
	{
		PushFreeNode(reinterpret_cast<FreeNode*>(slot_ptr));
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

// All small slots live in one contiguous reserved region, so membership is a
// range check plus a slot-alignment check plus a live-bit test.
bool MidoriAllocator::Contains(const void* ptr) const noexcept
{
	const size_t offset = static_cast<size_t>(reinterpret_cast<uintptr_t>(ptr) - reinterpret_cast<uintptr_t>(m_region_base));
	if (offset < m_committed_bytes)
	{
		const size_t block_offset = offset % BLOCK_SIZE;
		if (block_offset % SLOT_SIZE != 0uz || block_offset >= USABLE_BLOCK_BYTES)
		{
			return false;
		}

		const size_t slot_index = block_offset / SLOT_SIZE;
		const size_t word_index = (offset / BLOCK_SIZE) * LIVE_WORDS_PER_BLOCK + (slot_index / 64uz);
		const uint64_t mask = 1ull << (slot_index % 64uz);
		return (m_live_bits[word_index] & mask) != 0ull;
	}

	return ContainsLargeAllocation(ptr);
}

bool MidoriAllocator::SetLiveBit(void* ptr, bool is_live) noexcept
{
	const size_t offset = static_cast<size_t>(reinterpret_cast<uintptr_t>(ptr) - reinterpret_cast<uintptr_t>(m_region_base));
	if (offset >= m_committed_bytes)
	{
		return false;
	}

	const size_t block_offset = offset % BLOCK_SIZE;
	if (block_offset % SLOT_SIZE != 0uz || block_offset >= USABLE_BLOCK_BYTES)
	{
		return false;
	}

	const size_t slot_index = block_offset / SLOT_SIZE;
	const size_t word_index = (offset / BLOCK_SIZE) * LIVE_WORDS_PER_BLOCK + (slot_index / 64uz);
	const uint64_t mask = 1ull << (slot_index % 64uz);
	if (is_live)
	{
		m_live_bits[word_index] |= mask;
	}
	else
	{
		m_live_bits[word_index] &= ~mask;
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
