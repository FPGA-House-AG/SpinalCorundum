#include <stdint.h>
#define raw_local_irq_save(flags)			\
	do {					            	\
		typecheck(unsigned long, flags);	\
		flags = arch_local_irq_save();		\
	} while (0)
#define raw_local_irq_restore(flags)    	\
	do {						            \
		typecheck(unsigned long, flags);	\
		raw_check_bogus_irq_restore();		\
		arch_local_irq_restore(flags);		\
	} while (0)

struct noise_replay_counter {
    u64 counter;
    spinlock_t lock;
    unsigned long backtrack[COUNTER_BITS_TOTAL / BITS_PER_LONG];
};

static inline int test_and_set_bit(unsigned long nr, volatile unsigned long *addr)
{
    smp_mb__before_atomic();
    return test_and_set_bit_lock(nr, addr);
}

int __mips_test_and_set_bit_lock(unsigned long nr,
				 volatile unsigned long *addr)
{
	volatile unsigned long *a = &addr[BIT_WORD(nr)];
	unsigned int bit = nr % BITS_PER_LONG;
	unsigned long mask;
	unsigned long flags;
	int res;

	mask = 1UL << bit;
	raw_local_irq_save(flags);
	res = (mask & *a) != 0;
	*a |= mask;
	raw_local_irq_restore(flags);
	return res;
}

static inline int test_and_set_bit_lock(unsigned long nr, volatile unsigned long *addr)
{
        volatile unsigned long *m = &addr[BIT_WORD(nr)];
        int bit = nr % BITS_PER_LONG;
        unsigned long res, orig;

        if (!kernel_uses_llsc) {
            res = __mips_test_and_set_bit_lock(nr, addr);
        } else {
            orig = __test_bit_op(*m, "%0",
                    "or\t%1, %0, %3",
                    "ir"(BIT(bit)));
            res = (orig & BIT(bit)) != 0;
        }

        smp_llsc_mb();

        return res;
}

    if (unlikely((COUNTER_WINDOW_SIZE + their_counter) <
		     counter->counter))
		goto out;

	index = their_counter >> ilog2(BITS_PER_LONG);

	if (likely(their_counter > counter->counter)) {
		index_current = counter->counter >> ilog2(BITS_PER_LONG);
		top = min_t(unsigned long, index - index_current,
			    COUNTER_BITS_TOTAL / BITS_PER_LONG);
		for (i = 1; i <= top; ++i)
			counter->backtrack[(i + index_current) &
				((COUNTER_BITS_TOTAL / BITS_PER_LONG) - 1)] = 0;
		counter->counter = their_counter;
	}

	index &= (COUNTER_BITS_TOTAL / BITS_PER_LONG) - 1;
	ret = !test_and_set_bit(their_counter & (BITS_PER_LONG - 1),
				&counter->backtrack[index]);