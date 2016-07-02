// #include <stdlib.h>
#include "pthread.h"
// #include <string.h>

char *v;

void *thread1(void * arg)
{
  return; 
}

int main()
{
  pthread_t t1;

  pthread_create(&t1, 0, thread1, 0);
  pthread_join(t1, 0);

  return 0;
}

