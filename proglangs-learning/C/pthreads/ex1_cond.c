#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

pthread_mutex_t count_mutex     = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  condition_var   = PTHREAD_COND_INITIALIZER;

void *functionCount1();
void *functionCount2();
int  count = 0;
#define COUNT_DONE  10
#define COUNT_HALT1  3
#define COUNT_HALT2  6

int main()
{
   pthread_t thread1, thread2, thread3;
      int i1=1, i3=3;

   pthread_create( &thread1, NULL, &functionCount1, &i1);
   pthread_create( &thread2, NULL, &functionCount2, NULL);
   pthread_create( &thread3, NULL, &functionCount1, &i3);

   pthread_join( thread1, NULL);
   pthread_join( thread2, NULL);
   pthread_join( thread3, NULL);

   printf("Final count: %d\n",count);

   return 0;
}

// Write numbers 1-3 and 8-10 as permitted by functionCount2()

void *functionCount1(void *i)
{
   for(;;)
   {
      // Lock mutex and then wait for signal to relase mutex
      pthread_mutex_lock( &count_mutex );

      // Wait while functionCount2() operates on count
      // mutex unlocked if condition varialbe in functionCount2() signaled.
      pthread_cond_wait( &condition_var, &count_mutex );
      // printf("lock1\n");
      count++;
      printf("Counter value functionCount%d: %d\n", *((int *) i), count);

      // printf("unlock1\n");
      pthread_mutex_unlock( &count_mutex );

      if(count >= COUNT_DONE) return(NULL);
    }
}

// Write numbers 4-7

void *functionCount2()
{
    for(;;)
    {
       pthread_mutex_lock( &count_mutex );
       // printf("lock2\n");

       if( count < COUNT_HALT1 || count > COUNT_HALT2 )
       {
          // Condition of if statement has been met.
          // Signal to free waiting thread by freeing the mutex.
          // Note: functionCount1() is now permitted to modify "count".
          pthread_cond_broadcast( &condition_var );
       }
       else
       {
          count++;
          printf("Counter value functionCount2: %d\n",count);
       }

       // printf("unlock2\n");
       pthread_mutex_unlock( &count_mutex );

       if(count >= COUNT_DONE) return(NULL);
    }

}
