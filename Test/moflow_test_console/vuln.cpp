#include <Windows.h>
#include <stdio.h>


struct overflow_struct
{
	char buf[128];
};


void hook_function()
{
	printf("I'm hooked by a hax0r!\n");
}

void overwrite_retval()
{
	int var1 = 0;
	int *ptr = &var1;
	ptr += 2;
	*ptr = (int)&hook_function;
}


_declspec(dllexport) int test_direct_overwrite()
{
	printf("  test_direct_overwrite()\n");
	overwrite_retval();
	return 0;
}


int test_stack_buffer_overflow(char *data)
{
	overflow_struct overflow;

	printf("  test_stack_buffer_overflow()\n");
	puts((char*)data);  
	strncpy((char *)&overflow.buf, data, 512);

	return 0;
}