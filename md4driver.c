#include "MD4.h"
#include <unistd.h>
#define ANSWER 42

typedef unsigned char byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);


int read_cmd(byte *buf)
{
  int len;

  if(read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;
  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do
  {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  }while(got<len);

  return len;
}

int write_exact(byte *buf, int len)
{
  int i, wrote=0;

  do
  {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  }while(wrote<len);

  return len;
}


//message is the rpc from the erlang port
//is of the form <<Size:2, Data:Size, _Rest/binary>>
//Size is little-endian
//returns -1 if message is an {'EXIT', Why} message



int main(void)
{
  int i;
  byte buf[1024];
  byte out[16];
  int buflen = 0;
  for(i=0;i<1024;i++)
    buf[i] = 0;
  for(i=0;i<16;i++)
    out[i] = 16;
  while((buflen = read_cmd(buf)) > 0)
  {
    auth_md4Sum(out, buf, buflen);
    write_cmd(out, 16);
    for(i=0;i<1024;i++)
      buf[i]=0;
    for(i=0;i<16;i++)
      out[i]=0;
  }
}
