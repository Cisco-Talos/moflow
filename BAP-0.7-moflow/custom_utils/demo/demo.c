#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define BUF_SIZE 1024
#define BIG 0x1000000

void read_file(char *fn, char *buffer, int size){
  FILE * pFile;
  size_t result;

  pFile = fopen (fn, "rb" );
  if (pFile==NULL) {fprintf(stderr, "Can't open %s", fn); exit (1);}

  result = fread (buffer,1,size,pFile);
  if (result != size) 
    printf("read bytes %d < %d\n", result, size);

  fclose (pFile);
}

void test_control_dep(char *buf){
  char b0, b1, b2;
  int size;
  char *ptr;

  b0 = buf[0];
  b1 = buf[1];

  if(b0 == '0'){
    if(b1 == '1'){
      b2=2;
    }
    else{
      b2=3;
    }
    b2=b2;
    size = 16;
  }
  else{
    size = BIG;
  }
  printf("size=%d\n", size);
  ptr = (char*)malloc(size);
  ptr[BIG-1] = 0;
}

struct header {
  char magic[4];
  int total_size; // read av on large total_size
  int record_count;
};

struct record {
  int type;     // read or subrecord
  int len;      // write av on long len 
  char val[1];
};

void test_tlv_triage(char *buf)
{
  char *newbuf, *r_copy;
  int count, offset;
  struct header *h = (struct header *)buf;
  struct record *r = (struct record *)(buf + sizeof(struct header));

  if(memcmp(&h->magic, "AAAA", 4) != 0)
  {
    printf("bad magic\n");
    return;
  }
  else
  {
    printf("total_size: %d, record_count %d\n", h->total_size, h->record_count);
  }

  newbuf = (char *)malloc(BUF_SIZE);
  memcpy(newbuf, buf, h->total_size); // readAV if total_size > BUF_SIZE

  count = 0;
  offset = sizeof(struct header);
  while(count < h->record_count)
  {
    printf("record: type %d, len %d\n", r->type, r->len);
    if(r->type == 1)
    {
      memcpy(newbuf, r->val, r->len); // readav?
    }
    else if(r->type == 2)
    {
      memcpy(newbuf + (int)&r->val, r->val + 4, r->len);
    }

    offset += r->len;
    count++;
  }

  return;
}

void exploit_me(int depth, unsigned int x, unsigned int y){
  int stack[1];
  int b, i;

  b = x & 0xff;

  switch(depth){
    case 4:
      if(b == 0x44)
        stack[y] = 1;
      for(i=0;i<4;i++) stack[i] = 0x29a;
      return;
    case 3:
      if(b != 0x33) y = 0;
      break;
    case 2:
      if(b != 0x22) y = 0;
      break;
    case 1:
      if(b != 0x11) y = 0;
      break;
    default:
      assert(0);
  }
  exploit_me(++depth, x>>8, y);
}

void test_motriage(unsigned int *buf){
  unsigned int b,x,y;

  b = buf[0];
  x = buf[b+0x11223344];
  y = buf[x];
  exploit_me(1, x, y);
}

int main(int argc, char *argv[]){
  char *type;
  char buf[BUF_SIZE];

  if(argc != 3){
    printf("Usage: %s <cdep|recursive|tlv> <input_file>\n", argv[0]);
    return 1;
  }

  memset(buf, 0, sizeof(char)*BUF_SIZE);
  read_file(argv[2], buf, BUF_SIZE);
   
  type = argv[1];
  if(strcmp(type, "cdep") == 0){
    test_control_dep(buf);
  }
  else if(strcmp(type, "tlv") == 0){
    test_tlv_triage(buf);
  }
  else if(strcmp(type, "recursive") == 0){
    test_motriage((unsigned int*)buf);
  }
  else{
    printf("No such test: %s\n", type);
    return 1;
  }

  return 0;
}
