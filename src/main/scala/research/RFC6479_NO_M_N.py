import random
import math

def rfc6479():

    W            = 128
    N            = 8
    M            = 4
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

    values = []
    results = []
    
    file = open("/project-on-host/SpinalCorundum/outputs_sw.txt")
    lines = file.readlines()
    for line in lines:
        retvals = line.split(",")
        values.append(int(retvals[0]))
        results.append(int(retvals[1]))
    for j in range(len(values)):
        i = values[j]
        check = results[j]
        if(i == 65390):
            a=1
        S = bin(i)[2:].zfill(64) 
        Wt_ptr = (Wt) & (W - 1)
        Wb_ptr = (Wb) & (W - 1)

        if(int(S,2) == 0):       # Start of operations, or wrapped.
            #print("S=", int(S, 2), " DROP = 0 ", memory[::-1])
            assert(check == 1)
            continue

        elif(int(S,2) > Wt):     # New packet. Slide the window, accept the packet
            diff = int(S,2) - Wt
            
            #              block +1 , diff times
            for j in range(Wt+1, int(S,2)):
                memory[j % W] = '0' 

            Wt = int(S,2)
            Wt_ptr = (Wt) & (W - 1)
            memory[Wt_ptr] = '1'
            #print("S=", int(S,2), "DROP = 0, WINDOW SLIDES FORWARD BY ", diff, " ", memory[::-1])
            assert(check == 0)

            continue

        elif(int(S,2) + W < Wt): # Too old.
            #print("S=", int(S, 2), "DROP = 1 DUE TO TOO OLD PACKET ", memory[::-1])
            assert(check == 1)
            continue

        else:
            # S is inside of window. Check the bit in memory that S_ptr is pointing to.
            S_ptr = int(S,2) % W
            if(memory[S_ptr] == '0'):
            # We haven't seen this packet yet. We set the bit in memory, and don't update the window.
                memory[S_ptr] = '1'
                #print("S=", int(S, 2), "DROP = 0, INSIDE WINDOW ", memory[::-1])
                assert(check == 0)
            else:
            # We've seen this packet already, we drop it, and we don't update the window.
                #print("S=", int(S, 2), "DROP = 1, INSIDE WINDOW ", memory[::-1])
                assert(check == 1)

    #print(values)

  
rfc6479()
