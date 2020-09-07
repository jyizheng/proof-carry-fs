/*
The MIT License

Copyright (c) <2001,2008,2009> <Deepak Garg dg@cs.cmu.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*/

#ifndef __THREAD_HPP
#define __THREAD_HPP

#include <pthread.h>
#include <semaphore.h>

class Semaphore {
	private:
	
	sem_t * sem;
	public:
	Semaphore(int maxUsage) {
		sem = new sem_t;
		sem_init(sem,0,maxUsage);
		}

	void wait() {
		sem_wait(sem);
		}

	void signal() {
		sem_post(sem);
		}

	int isopen() {
		int i;
		sem_getvalue(sem,&i);
		return i;
		}

	~Semaphore() {
		sem_destroy(sem);
		delete sem;
		}
};
	
class Mutex {
	private:
	
	pthread_mutex_t * mut;
	public:
	Mutex() {
		mut = new pthread_mutex_t;
		pthread_mutex_init(mut,NULL);
		}

	void lock() {
		pthread_mutex_lock(mut);
		}

	void unlock() {
		pthread_mutex_unlock(mut);
		}

	int trylock() {
		int i;
		i = pthread_mutex_trylock(mut);
		return !i;
		}

	~Mutex() {
		pthread_mutex_destroy(mut);
		delete mut;
		}
};

class Condition {
	pthread_cond_t * cond;
	pthread_mutex_t * mut;

	public:
	Condition() {
		cond = new pthread_cond_t;
		pthread_cond_init(cond,NULL);
		mut = new pthread_mutex_t;
		pthread_mutex_init(mut,NULL);
		}

	void signal(int c) {
		pthread_mutex_lock(mut);
		if (c) pthread_cond_signal(cond);
		pthread_mutex_unlock(mut);
		}

	void broadcast(int c) {
		pthread_mutex_lock(mut);
		if (c) pthread_cond_broadcast(cond);
		pthread_mutex_unlock(mut);
		}
	
	void wait(int c) const {
		pthread_mutex_lock(mut);
		if (c) pthread_cond_wait(cond,mut);
		pthread_mutex_unlock(mut);
		}

	~Condition() {
		pthread_cond_destroy(cond);
		pthread_mutex_destroy(mut);
		delete cond;
		delete mut;
		}
};
	
class Thread {
	private:
	pthread_t * th;
	pthread_attr_t * attr;
	int started,stopped;
	pthread_mutex_t mutex;

	static void * init_run(void * arg) {
		pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL);
 		((Thread *)arg)->run();
		return NULL;
	}
	
	public:
	
	Thread() {
		th=new pthread_t;
		attr=NULL;
		started = stopped = 0;
		pthread_mutex_init(&mutex,NULL);
	}
	
	virtual void run() = 0;
	
	int start() {
		pthread_mutex_lock(&mutex);
		if (!started && !stopped) {
			started=1;
			pthread_create(th,NULL,&(Thread::init_run),this);
			pthread_mutex_unlock(&mutex);
			return 1;
		}			
		pthread_mutex_unlock(&mutex);
		return 0;
	}

	void stop() {
		pthread_mutex_lock(&mutex);
		if (!stopped && started) {
			stopped = 1;
			pthread_cancel(*th);
			}
		pthread_mutex_unlock(&mutex);
	}

	int join() {
		void ** r = new void*;
		int ret = pthread_join(*th,r);
		delete r;
		return (!ret);
	}

	~Thread() {
		delete th;
		pthread_mutex_destroy(&mutex);
	}
};

#endif
