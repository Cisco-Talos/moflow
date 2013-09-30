#include <windows.h>
#include <stdio.h>
#include <fstream>

using namespace std;

int test_stack_buffer_overflow(char *data);

int generate_test_file()
{
	char *filename = "moflow_test_file.txt";
	fstream *file;
	char buf[8192];

	printf("  generate_test_file()\n");

	memset(buf, 'A', sizeof(buf));
	buf[sizeof(buf) - 1] = 0;

	file = new fstream(filename, ios::out);
	if(file == NULL)
	{
		printf("error creating %s. exiting.\n", filename);
		exit(1);
	}
	file->write(buf, sizeof(buf));
	file->close();

	return 0;
}

void taint_dummy(char *buf)
{
	int count = 0;
	char letter = 0;

	letter = buf[0];
	count = (int)letter;
	printf("Counting ... ");
	for(int i = 0; i < count; i++)
	{
		printf("%d.. ", i);
		Sleep(100);
	}
}

int test_file_io_ansi()
{
	char *filename = "moflow_test_file.txt";
	FILE *file;
	char buf[8192];

	printf(" test_file_io()\n");

    if(GetFileAttributesA(filename) == -1)
    	generate_test_file();

	file = fopen(filename, "r");
	if(file == NULL)
	{
		printf("error opening %s. exiting.\n", filename);
		exit(1);
	}

	while(!feof(file))
		fread(buf, sizeof(buf) - 1, 1, file);
	fclose(file);
	
	Sleep(1000);

	if(buf[0] == 'A')
	{
		taint_dummy(buf);
		test_stack_buffer_overflow(buf);
	}	

	return 0;
}

int test_file_io_cpp()
{
	char *filename = "moflow_test_file.txt";
	fstream *file;
	char buf[8192];

	printf(" test_file_io()\n");

    if(GetFileAttributesA(filename) == -1)
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

int test_file_io()
{
	return test_file_io_ansi();
}