import random
import math

def rfc6479():

    W            = 8
    #N            = 8
    #M            = 4
    Wt           = 0
    Wb           = Wt - W + 1
    NUM_SESSIONS = 8 

    random.seed(123)
    memory    = ['0']*W
    S_base    = 0
    variation = 8
    swapped   = list(range(100, 150))
    for _ in range(40):
        index1 = random.randint(0, len(swapped)-1)
        index2 = random.randint(0, len(swapped)-1)
        # Swap the elements at the randomly chosen indices
        swapped[index1], swapped[index2] = swapped[index2], swapped[index1]

    values = [0, 0, 0, 0, 52, 53, 57, 59, 60, 62, 60, 63, 64, 67, 69, 71, 74, 75, 79, 80, 81, 82, 84, 85, 86, 87, 89, 90, 92, 93, 96, 97, 99, 125, 110, 118, 135, 127, 106, 128, 107, 101, 142, 115, 120, 112, 134, 114, 108, 136, 126, 100, 144, 130, 119, 122, 103, 138, 104, 123, 132, 102, 129, 133, 113, 149, 116, 143, 117, 146, 137, 131, 139, 109, 148, 140, 124, 121, 145, 111, 147, 105, 141]#list(range(0)) + [x for i, x in enumerate(list(range(50, 100))) if i not in random.sample(range(len(list(range(50, 100)))), 20)]  + swapped   
    for i in values:
        if(i == 110):
            a=1
        S = bin(i)[2:].zfill(64) 
        Wt_ptr = (Wt) & (W - 1)
        Wb_ptr = (Wb) & (W - 1)

        if(int(S,2) == 0):       # Start of operations, or wrapped.
            print("S=", int(S, 2), " DROP = 0 ", memory[::-1])
            continue

        elif(int(S,2) > Wt):     # New packet. Slide the window, accept the packet
            diff = int(S,2) - Wt
            

            for j in range(Wt+1, int(S,2)):
                memory[j % W] = '0' 

            Wt = int(S,2)
            Wt_ptr = (Wt) & (W - 1)
            memory[Wt_ptr] = '1'
            print("S=", int(S,2), "DROP = 0, WINDOW SLIDES FORWARD BY ", diff, " ", memory[::-1])

            continue

        elif(int(S,2) + W < Wt): # Too old.
            print("S=", int(S, 2), "DROP = 1 DUE TO TOO OLD PACKET ", memory[::-1])
            continue

        else:
            # S is inside of window. Check the bit in memory that S_ptr is pointing to.
            S_ptr = int(S,2) % W
            if(memory[S_ptr] == '0'):
            # We haven't seen this packet yet. We set the bit in memory, and don't update the window.
                memory[S_ptr] = '1'
                print("S=", int(S, 2), "DROP = 0, INSIDE WINDOW ", memory[::-1])
            else:
            # We've seen this packet already, we drop it, and we don't update the window.
                print("S=", int(S, 2), "DROP = 1, INSIDE WINDOW ", memory[::-1])
    print(values)

  
rfc6479()
