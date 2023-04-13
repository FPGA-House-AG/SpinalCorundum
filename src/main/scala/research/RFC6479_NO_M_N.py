import random
import math

def rfc6479():

    W            = 32
    #N            = 8
    #M            = 4
    Wt           = 0
    Wb           = Wt - W + 1
    NUM_SESSIONS = 8

    random.seed(123)
    memory    = ['0']*32
    S_base    = 0
    variation = 8
    swapped   = list(range(100, 150))
    for _ in range(40):
        index1 = random.randint(0, len(swapped)-1)
        index2 = random.randint(0, len(swapped)-1)
        # Swap the elements at the randomly chosen indices
        swapped[index1], swapped[index2] = swapped[index2], swapped[index1]

    values = list(range(50)) + [x for i, x in enumerate(list(range(50, 100))) if i not in random.sample(range(len(list(range(50, 100)))), 20)]  + swapped   
    for i in values:
        if i == 118:
            a = 1
        if i == 99:
            b = 1
        #S = ''.join(random.choice(['0', '1'])for _ in range(64))
        S = bin(i)[2:].zfill(64) 
        Wt_ptr = (Wt) & (W - 1)
        Wb_ptr = (Wb) & (W - 1)

        if(int(S,2) == 0):       # Start of operations, or wrapped.
            print("S=", int(S, 2), " DROP = 0")
            continue

        elif(int(S,2) > Wt):     # New packet. Slide the window, accept the packet
            diff = int(S,2) - Wt
            

            for i in range(Wt+1, int(S,2)):
                memory[i & (W-1)] = '0' 

            print("S=", int(S,2), "DROP = 0, WINDOW SLIDES FORWARD BY ", diff)
            
            Wt = int(S,2)
            Wt_ptr = (Wt) & (W - 1)
            memory[Wt_ptr] = '1'
            
            continue

        elif(int(S,2) + W < Wt): # Too old.
            print("S=", int(S, 2), "DROP = 1 DUE TO TOO OLD PACKET")
            continue

        else:
            # S is inside of window. Check the bit in memory that S_ptr is pointing to.
            S_ptr = int(S,2) & (W - 1)
            if(memory[S_ptr] == '0'):
            # We haven't seen this packet yet. We set the bit in memory, and don't update the window.
                memory[S_ptr] = '1'
                print("S=", int(S, 2), "DROP = 0, INSIDE WINDOW")
            else:
            # We've seen this packet already, we drop it, and we don't update the window.
                print("S=", int(S, 2), "DROP = 1, INSIDE WINDOW")


  
rfc6479()
