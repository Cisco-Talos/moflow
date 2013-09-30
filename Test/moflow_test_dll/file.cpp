#include <windows.h>
#include <stdio.h>
#include <fstream>

using namespace std;

int test_stack_buffer_overflow(char *data);

int generate_test_file()
{
	char *filename = "moflow_test_file.txt";
	fstream *file;
	char buf[1024];

	printf("  generate_test_file()\n");

	memset(buf, 'A', 1024);
	buf[sizeof(buf) - 1] = 0;

	file = new fstream(filename, ios::out);
	if(file == NULL)
	{
		printf("error creating %s. exiting.\n", filename);
		exit(1);
	}
	file->write(buf, 1024);
	file->close();

	return 0;
}

__declspec(dllexport) int dll_test_file_io()
{
	char *filename = "moflow_test_file.txt";
	fstream *file;
	char buf[1024];

	printf(" test_file_io()\n");

    if(GetFileAttributes(filename) == -1)
    	generate_test_file();

	file = new fstream(filename, ios::in|ios::binary);
	if(file == NULL)
	{
		printf("error opening %s. exiting.\n", filename);
		exit(1);
	}

	while(!file->eof())
		file->read(buf, 1024);
	file->close();
	
	Sleep(1000);

	test_stack_buffer_overflow(buf);

	return 0;
}