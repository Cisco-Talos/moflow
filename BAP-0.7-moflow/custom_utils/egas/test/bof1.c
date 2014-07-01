#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void crash(){
  int i;
  // Add some basic blocks
  for(i=0;i<10;i++){
    i += 1;
  }
  *(int*)NULL = 0;
}

void test(char * buf)
{
    int n=0;
    if(buf[0] == 'b') n++;
    if(buf[1] == 'a') n++;
    if(buf[2] == 'd') n++;
    if(buf[3] == '!') n++;
    if(n==4){
        crash();
    }
}

int fd;

int main(int argc, char *argv[])
{
  char buf[500];
  size_t count;

  fd = open(argv[1], O_RDONLY);
  if(fd == -1) {
    perror("open");
    exit(-1);
  }
  count = read(fd, buf, 500);
  if(count == -1) {
    perror("read");
    exit(-1);
  }
  close(fd);
  
  test(buf);

  return 0;
}
