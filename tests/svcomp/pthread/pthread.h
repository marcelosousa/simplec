typedef unsigned long int pthread_t;

typedef struct
{
    int volatile value;
} pthread_mutex_t;

typedef struct
{
    int flags;
    void * stack_base;
    int stack_size;
    int guard_size;
    int sched_policy;
    int sched_priority;
} pthread_attr_t;

typedef union
{
  struct
  {
    int __lock;
    unsigned int __futex;
    __extension__ unsigned long long int __total_seq;
    __extension__ unsigned long long int __wakeup_seq;
    __extension__ unsigned long long int __woken_seq;
    void *__mutex;
    unsigned int __nwaiters;
    unsigned int __broadcast_seq;
  } __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;

typedef unsigned long int pthread_mutexattr_t;
typedef union {
            char __size[56]; long int __align;
        } pthread_attr_t;

int pthread_create(pthread_t *thread, const pthread_attr_t *attr, void *(*start_routine)(void*), void *arg);
int pthread_join(pthread_t thread, void **arg);
int pthread_mutex_init(pthread_mutex_t *lock, const pthread_mutexattr_t *a);
int pthread_mutex_lock(pthread_mutex_t *lock);
int pthread_mutex_unlock(pthread_mutex_t *lock);
int printf(const char * restrict format, ...);