/**
* Copyright (c) 2012 IETF Trust and the persons identified as
* authors of the code. All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, is permitted pursuant to, and subject to the license
* terms contained in, the Simplified BSD License set forth in Section
* 4.c of the IETF Trust’s Legal Provisions Relating to IETF Documents
* (http://trustee.ietf.org/license-info).
*
*/

/**
* In this algorithm, the hidden window size must be a power of two,
* for example, 1024 bits. The redundant bits must also be a power of
* two, for example 32 bits. Thus, the supported anti-replay window
* size is the hidden window size minus the redundant bits. It is 992
* in this example. The size of the integer depends on microprocessor
* architecture. In this example, we assume that the software runs on
* a 32-bit microprocessor. So the size of the integer is 32. In order
* to convert the bitmap into an array of integers, the total number of
* integers is the hidden window size divided by the size of the
* integer.
*
* struct ipsec_sa contains the window and window related parameters,
* such as the window size and the last acknowledged sequence number.
*
* all the value of macro can be changed, but must follow the rule
* defined in the algorithm.
*/

#include <stdint.h>                 //Added new
#define SIZE_OF_INTEGER 32 /** 32-bit microprocessor */
#define BITMAP_LEN (1024/ SIZE_OF_INTEGER) /** in terms of the 32-bit integer */
#define IPSEC_BITMAP_LEN BITMAP_LEN //Added new
#define IPSEC_REDUNDANT_BITS 32     //Added new
#define BITMAP_INDEX_MASK (IPSEC_BITMAP_LEN-1)
#define REDUNDANT_BIT_SHIFTS 5      //Added new
#define REDUNDANT_BITS (1<<REDUNDANT_BIT_SHIFTS)
#define BITMAP_LOC_MASK (IPSEC_REDUNDANT_BITS-1)

struct ipsec_sa{
    int replaywin_size;
    int replaywin_lastseq;
    int replaywin_bitmap[];
}; //Added new

int ipsec_check_replay_window (struct ipsec_sa *ipsa, uint32_t sequence_number)
{
    int bit_location;
    int index;

    /**
    * replay shut off
    */
    if (ipsa->replaywin_size == 0) {
        return 1;
    }

    /**
    * first == 0 or wrapped
    */
    if (sequence_number == 0) {
        return 0;
    }

    /**
    * first check if the sequence number is in the range
    */
    if (sequence_number>ipsa->replaywin_lastseq) {
        return 1; /** larger is always good */
    }

    /**
    * The packet is too old and out of the window
    */
    if ((sequence_number + ipsa->replaywin_size) < ipsa->replaywin_lastseq) { 
       return 0;
    }

    /**
    * The sequence is inside the sliding window
    * now check the bit in the bitmap
    * bit location only depends on the sequence number
    */
    bit_location = sequence_number & BITMAP_LOC_MASK;
    index = (sequence_number >> REDUNDANT_BIT_SHIFTS) & BITMAP_INDEX_MASK;

    /*
    * this packet has already been received
    */
    if (ipsa->replaywin_bitmap[index] & (1 << bit_location)) {
        return 0;
    }
    
    return 1;
}

int ipsec_update_replay_window (struct ipsec_sa *ipsa, uint32_t sequence_number)
{
    int bit_location;
    int index, index_cur, id;
    int diff;

    if (ipsa->replaywin_size == 0) {
        return 1;
    }
    /** replay shut off */
    if (sequence_number == 0) {
        return 0;
    /** first == 0 or wrapped */
    }

    /**
    * the packet is too old, no need to update
    */
    if ((ipsa->replaywin_size + sequence_number) < ipsa->replaywin_lastseq) {
        return 0;
    }

    /**
    * now update the bit
    */
    index = (sequence_number>>REDUNDANT_BIT_SHIFTS);

    /**
    * first check if the sequence number is in the range
    */
    if (sequence_number>ipsa->replaywin_lastseq) {

        index_cur = ipsa->replaywin_lastseq>>REDUNDANT_BIT_SHIFTS;
        diff = index - index_cur;

        if (diff > BITMAP_LEN) { /* something unusual in this case */
            diff = BITMAP_LEN;
        }

        for (id = 0; id < diff; ++id) {
            ipsa->replaywin_bitmap[(id+index_cur+1)&BITMAP_INDEX_MASK] = 0;
        }

        ipsa->replaywin_lastseq = sequence_number;
    }

    index &= BITMAP_INDEX_MASK;
    bit_location = sequence_number&BITMAP_LOC_MASK;

    /* this packet has already been received */
    if (ipsa->replaywin_bitmap[index] & (1 << bit_location)) {
        return 0;
    }

    ipsa->replaywin_bitmap[index] |= (1 << bit_location);
    return 1;
}




