/*
 * Copyright (c) 2007, 2008 University of Tsukuba
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of the University of Tsukuba nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "assert.h"
#include "initfunc.h"
#include "linkage.h"
#include "list.h"
#include "mm.h"
#include "panic.h"
#include "pcpu.h"
#include "spinlock.h"
#include "string.h"
#include "thread.h"
#include "thread_switch.h"

#define MAXNUM_OF_THREADS	256
#define CPUNUM_ANY		-1

extern ulong volatile syscallstack asm ("%gs:gs_syscallstack");

struct thread_context {
	ulong r12, r13, r14_edi, r15_esi, rbx, rbp, rip;
};

struct thread_data {
	LIST1_DEFINE (struct thread_data);
	struct thread_context *context;
	tid_t tid;
	int cpunum;
	bool boot;
	void *stack;
	int pid;
	ulong syscallstack;
	phys_t process_switch;
	bool process_switch_enable;
};

static struct thread_data td[MAXNUM_OF_THREADS];
static LIST1_DEFINE_HEAD (struct thread_data, td_free);
static LIST1_DEFINE_HEAD (struct thread_data, td_runnable);
static spinlock_t thread_lock;

static void
thread_data_init (struct thread_data *d, struct thread_context *c, void *stack,
		  int cpunum)
{
	d->context = c;
	d->cpunum = cpunum;
	d->boot = false;
	d->stack = stack;
	d->pid = 0;
	d->syscallstack = 0;
	d->process_switch_enable = false;
}

static void
thread_data_save (struct thread_data *d)
{
	d->stack = currentcpu->stackaddr;
	d->pid = currentcpu->pid;
	d->syscallstack = syscallstack;
}

static void
thread_data_load (struct thread_data *d)
{
	currentcpu->stackaddr = d->stack;
	currentcpu->pid = d->pid;
	syscallstack = d->syscallstack;
	if (d->process_switch_enable)
		mm_process_switch (d->process_switch);
}

tid_t
thread_gettid (void)
{
	return currentcpu->tid;
}

static void
switched (void)
{
	spinlock_unlock (&thread_lock);
}

void
schedule (void)
{
	struct thread_data *d;
	tid_t oldtid, newtid;

	spinlock_lock (&thread_lock);
	LIST1_FOREACH (td_runnable, d) {
		if (d->cpunum == CPUNUM_ANY ||
		    d->cpunum == currentcpu->cpunum)
			goto found;
	}
	spinlock_unlock (&thread_lock);
	return;
found:
	LIST1_DEL (td_runnable, d);
	oldtid = currentcpu->tid;
	newtid = d->tid;
	thread_data_save (&td[oldtid]);
	currentcpu->tid = newtid;
	thread_data_load (d);
	LIST1_ADD (td_runnable, &td[oldtid]);
	thread_switch (&td[oldtid].context, d->context, 0);
	switched ();
}

ulong
thread_switch_to (ulong arg, tid_t tid)
{
	struct thread_data *d;
	tid_t oldtid, newtid;
	ulong r;

	d = &td[tid];
	spinlock_lock (&thread_lock);
	oldtid = currentcpu->tid;
	newtid = tid;
	thread_data_save (&td[oldtid]);
	currentcpu->tid = newtid;
	thread_data_load (d);
	r = thread_switch (&td[oldtid].context, d->context, arg);
	switched ();
	return r;
}

ulong
thread_switch_to_auto (void)
{
	struct thread_data *d;
	tid_t oldtid, newtid;
	ulong r;

	spinlock_lock (&thread_lock);
	LIST1_FOREACH (td_runnable, d) {
		if (d->cpunum == CPUNUM_ANY ||
		    d->cpunum == currentcpu->cpunum)
			goto found;
	}
	spinlock_unlock (&thread_lock);
	return 0;
found:
	oldtid = currentcpu->tid;
	newtid = d->tid;
	thread_data_save (&td[oldtid]);
	currentcpu->tid = newtid;
	thread_data_load (d);
	r = thread_switch (&td[oldtid].context, d->context, 0);
	switched ();
	return r;
}

asmlinkage void
thread_start1 (void (*func) (void *), void *arg)
{
	switched ();
	func (arg);
	thread_exit ();
}

static void
thread_new0 (struct thread_context *c, void *stack)
{
	struct thread_data *d;

	spinlock_lock (&thread_lock);
	d = LIST1_POP (td_free);
	ASSERT (d);
	thread_data_init (d, c, stack, CPUNUM_ANY);
	LIST1_ADD (td_runnable, d);
	spinlock_unlock (&thread_lock);
}

void
thread_new (void (*func) (void *), void *arg, int stacksize)
{
	u8 *stack, *q;
	struct thread_context c;

	stack = alloc (stacksize);
	q = stack + stacksize;
	c.rbp = 0;
	c.rip = (ulong)thread_start0;
#define PUSH(n) memcpy (q -= sizeof (n), &(n), sizeof (n))
	PUSH (arg);
	PUSH (func);
	PUSH (c);
#undef PUSH
	thread_new0 ((struct thread_context *)q, stack);
}

void
thread_exit (void)
{
	panic ("thread_exit");
}

void
thread_set_process_switch (phys_t switchto)
{
	td[currentcpu->tid].process_switch = switchto;
	td[currentcpu->tid].process_switch_enable = true;
}

static void
thread_init_global (void)
{
	int i;

	LIST1_HEAD_INIT (td_free);
	LIST1_HEAD_INIT (td_runnable);
	spinlock_init (&thread_lock);
	for (i = 0; i < MAXNUM_OF_THREADS; i++) {
		td[i].tid = i;
		LIST1_ADD (td_free, &td[i]);
	}
}

static void
thread_init_pcpu (void)
{
	struct thread_data *d;

	spinlock_lock (&thread_lock);
	d = LIST1_POP (td_free);
	ASSERT (d);
	thread_data_init (d, NULL, NULL, currentcpu->cpunum);
	d->boot = true;
	currentcpu->tid = d->tid;
	spinlock_unlock (&thread_lock);
}

INITFUNC ("global3", thread_init_global);
INITFUNC ("pcpu0", thread_init_pcpu);
