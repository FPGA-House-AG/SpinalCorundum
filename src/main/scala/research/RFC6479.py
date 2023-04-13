import random
import math

def rfc6479():

    W            = 8
    N            = 8
    M            = 4
    Wt           = 0
    Wb           = Wt - W + 1
    NUM_SESSIONS = 8

    # A % B = A & (B-1) when B % 2 == 0 and M * N is a power of 2.
    # Wb = Wt - W + 1

    #
    random.seed(123)
    memory = '01111111111111111111111111111111'#str('0' * (M * N))
    for i in range(1000):
        
        S = ''.join(random.choice(['0', '1'])for _ in range(64))
        
        Wt_ptr = (Wt) & (M * N - 1)
        Wb_ptr = (Wb) & (M * N - 1)

        if(int(S,2) == 0):       # Start of operations, or wrapped.
            print("DROP = 0")
            continue

        elif(int(S,2) > Wt):     # New packet. Slide the window, accept the packet
            Wt = int(S,2)
            Wb = Wt - W + 1
            Wt_ptr = (Wt) & (M * N - 1)
            Wb_ptr = (Wb) & (M * N - 1)
            #Slide the window and 0-initialize the needed bits.
            #We find the block of Wt_ptr is pointing to by doing Wt_ptr // N = Wt_ptr >> log2(N)
            #We find the # of bits to 0-initialize as N - (W_ptr mod N)
            block_number = (Wt_ptr) >> int(math.log2(N))
            value_index  = (Wt_ptr) & (N - 1)
            number_of_bits_to_init = (N) - value_index

            # Update the value to accept this packet
            updated_list = list(memory)
            updated_list[Wt_ptr] = '1'
            
            for i in range(1, number_of_bits_to_init):
                updated_list[block_number * N + value_index + i] = '0' 

            memory = ''.join(updated_list)
            print("DROP = 0")
            continue

        elif(int(S,2) + W < Wt): # Too old.
            print("DROP = 1 DUE TO TOO OLD PACKET")
            continue

        else:
            # S is inside of window. Check the bit in memory that S_ptr is pointing to.
            S_ptr = int(S,2) & (M * N - 1)
            if(memory[S_ptr] == '0'):
            # We haven't seen this packet yet. We set the bit in memory, and don't update the window.
                updated_list = list(memory)
                updated_list[S_ptr] = '1'
                memory = ''.join(updated_list)
                print("DROP = 0")
            else:
            # We've seen this packet already, we drop it, and we don't update the window.
                print("DROP = 1")


  
rfc6479()
