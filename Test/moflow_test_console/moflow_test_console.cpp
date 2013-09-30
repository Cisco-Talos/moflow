#include <Windows.h>
#include <stdio.h>
#include <time.h>

extern int test_file_io();
extern int test_network_io();
extern int test_direct_overwrite();

int main(int argc, char **argv)
{
	printf("Moflow test console v0.1\n");

	if(argc == 2)
	{
		if(!strcmp("file", argv[1]))
			test_file_io();
		else if(!strcmp("network", argv[1]))
			test_network_io();	
		else if(!strcmp("direct", argv[1]))
			test_direct_overwrite();
	}

	// pick a random testcase
	printf("Choosing random testcase..\n");
	srand(time(0));
	for(int i = 0; i < 10; i++)
		rand();

	if(rand() % 10 < 5)
		test_file_io();
	else
		test_network_io();

	return 0;
}