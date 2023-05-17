package rfc6479
import scala.math.log
import scala.math.ceil
import scala.collection.mutable.ArrayBuffer

class RFC6479_MN {
    var windowSizeValue = 128;
    var m               = 4;
    var n               = 32;
    var memory          = Array.fill[Int](windowSizeValue)(0)
    var counter         = -1;
    var counterSize     = 16;
    var resultArray     = ArrayBuffer[Int]()
    def log2Upper(n: Int): Int = ceil(log(n) / log(2)).toInt

    def test_and_set_bit(addr: Int): Boolean = {
        var old_val = this.memory(addr)
        if(old_val == 1){
            this.resultArray.append(3)
            return true;
        }
        else{
            this.memory(addr) = 1
            this.resultArray.append(4)
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
            this.resultArray.append(2)
            return true;
        }
        

        
        //new
        index = (their_counter+1) >> log2Upper(n)
        var newValueFlag:Boolean = false
        if((their_counter) > (this.counter)){
            index_current = this.counter >> log2Upper(n);

            if((index-index_current) < (windowSizeValue / n))
                top = (index-index_current)
            else
                top = (windowSizeValue / n)
            for(i <- 1 to top)
                for(j <-0 until n)
                    memory(((i+index_current)&((windowSizeValue/n) - 1))*n +j) = 0

            this.counter = their_counter; 
            //println(this.counter)
        }
        //new or inside
        var ret :Boolean = test_and_set_bit(their_counter % windowSizeValue)
        /*
        if(this.go == true){
            println(their_counter)
            println(this.memory.mkString(""))
        }
        if(their_counter == 65518){
            this.go = true
            //println(their_counter)
            //println(this.memory.mkString(""))
            //println(their_counter%windowSizeValue)
            //println(this.memory(their_counter%windowSizeValue))
        }
        if(their_counter == 65404)
        {this.go = false}
        */
        return ret
    }

        
    
}


object Main {
    def main(args: Array[String]){
        /*
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
        */
    }
}
