package rfc6479
import scala.math.log
import scala.math.ceil

class RFC6479_MN {
    var windowSizeValue = 128;
    var m               = 4;
    var n               = 32;
    var memory          = Array.fill[Int](windowSizeValue)(0)
    var counter         = -1;

    def log2Upper(n: Int): Int = ceil(log(n) / log(2)).toInt

    def test_and_set_bit(addr: Int): Boolean = {
        var old_val = this.memory(addr)
        if(old_val == 1){
            println("INSIDE AND ALREADY SEEN!");
            return true;
        }
        else{
            this.memory(addr) = 1
            println("NEW PACKET OR ALREADY INSIDE AND NOT ALREADY SEEN!")
            return false
        }
    }

    //if method returns true, we drop the packet.
    def counter_validate(their_counter: Int): Boolean = {
        var index         :Int = -1;
        var index_current :Int = -1;
        var top           :Int = -1;
        var i             :Int = -1;
        

        //too old 
        if(this.windowSizeValue + their_counter < this.counter){
            println("TOO OLD!")
            return true;
        }
        
        
        //new
        index = their_counter >> log2Upper(n)

        if((their_counter) > (this.counter)){
            index_current = this.counter >> log2Upper(n);

            if((index-index_current) < (windowSizeValue >> log2Upper(n)))
                top = (index-index_current)
            else
                top = (windowSizeValue >> log2Upper(n))
            for(i <- 0 until top)
                for(j <-0 until n)
                    memory(i*n+j) = 0

            this.counter = their_counter; 
        }
        
        //new or inside
        var ret :Boolean = test_and_set_bit(their_counter % windowSizeValue)

        return ret
    }

        
    
}


object Main {
    def main(args: Array[String]){
        val testme = new RFC6479_MN
        val testValues = Array(0, 1, 1, 9, 8, 7, 7, 128, 127, 127, 126, 2, 2, 144, 3, 144, 512, 385, 10, 384, 383, 386, 385, 0) 
        val retValues  = Array(0, 0, 1, 0, 0, 0, 1,   0,   0,   1,   0, 0, 1,   0, 1,   1,   0,   0,  1,   1,   1,   0,   1, 1)

        
        for(k <- 0 until testValues.length){
        
            println(k)
            var retval = testme.counter_validate(testValues(k))
            assert(retval == (retValues(k)==1))
            println("")
            for(i <-0 until testme.windowSizeValue){
                print(testme.memory(i))
            }
            
            println("")
        }
        
    }
}
