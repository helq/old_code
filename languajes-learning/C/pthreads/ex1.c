// compile with: gcc -lpthread -std=c99 -Wall ex1.c
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define NUM_THREADS     5

struct person
{
    int i;
    pthread_mutex_t *mutex;
};

void *TaskCode(void *argument)
{
   struct person *tid;

   tid = (struct person *) argument;
   char *hi[] = {"Hello ", "World! ", "It's ", "me, ", "thread"};

   pthread_mutex_lock( tid->mutex );

   for(int i=0; i<5; i++) {
       for(int j=0; j<2000; j++);
       printf(hi[i]);
   }

   printf(" %d!\n", tid->i);

   pthread_mutex_unlock( tid->mutex );

   /* optionally: insert more useful stuff here */

   return NULL;
}

int main(void)
{
   pthread_t threads[NUM_THREADS];

   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

   struct person *thread_args[NUM_THREADS];
   for(int i=0; i<NUM_THREADS; i++) {
       thread_args[i] = calloc(1, sizeof(struct person));
       (thread_args[i])->mutex = &mutex;
   }

   int rc, i;

   /* create all threads */
   for (i=0; i<NUM_THREADS; ++i) {
      (thread_args[i])->i = i;
      pthread_mutex_lock( &mutex );
      printf("In main: creating thread %d\n", i);
      pthread_mutex_unlock( &mutex );
      rc = pthread_create(&threads[i], NULL, TaskCode, (void *) thread_args[i]);
      assert(0 == rc);
   }

   /* wait for all threads to complete */
   for (i=0; i<NUM_THREADS; ++i) {
      rc = pthread_join(threads[i], NULL);
      assert(0 == rc);
   }

   exit(EXIT_SUCCESS);
}
