#include "MidoriAllocator.h"

#include <cstdlib>

void* MidoriAllocator::Allocate(size_t size)
{
	if (size == 0uz)
	{
		return nullptr;
	}
	return std::malloc(size);
}

void MidoriAllocator::Free(void* ptr)
{
	std::free(ptr);
}
