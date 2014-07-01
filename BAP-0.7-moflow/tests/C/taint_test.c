
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


void impossible()
{
   printf("Its impossible to execute this function\n");
}

void test(char * buf)
{
   char local[8];
   int i;
   strcpy(local, buf);
}

int fd;

int main(int argc, char *argv[])
{
  char   buf[500];
  size_t count;
  size_t index;

  fd = open("readme", O_RDONLY);
  if(fd == -1) {
    perror("open");
    exit(-1);
  }

  count = read(fd, buf, 500);
  if(count == -1) {
    perror("read");
    exit(-1);
  }

  index = atoi( (const char*)(buf) );

  printf("hello!\n");

  if(buf[index] != 'h') {
    printf("the file contents with index must start with h\n");
	close(fd);
    exit(-1);
  }

  close(fd);

  test(buf);
  return 0;
}
