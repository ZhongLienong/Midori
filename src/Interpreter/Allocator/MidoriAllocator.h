#pragma once

#include <cstddef>

class MidoriAllocator
{
public:
	MidoriAllocator() = default;
	~MidoriAllocator() = default;

	MidoriAllocator(const MidoriAllocator&) = delete;
	MidoriAllocator& operator=(const MidoriAllocator&) = delete;

	void* Allocate(size_t size);
	void Free(void* ptr);
};
