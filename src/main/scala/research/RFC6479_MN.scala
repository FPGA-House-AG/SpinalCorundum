package rfc6479
import scala.math.log
import scala.math.ceil
import scala.collection.mutable.ArrayBuffer

class RFC6479_MN(var p_counterSize: Int, var p_M: Int, var p_N: Int) {
    var windowSizeValue = p_M * p_N;
    var m               = p_M;
    var n               = p_N;
    var memory          = Array.ofDim[Int](m, n)
    var counter         = -1;
    var counterSize     = p_counterSize;
    var resultArray     = ArrayBuffer[Int]()
    var s_plus_1        = -1
    var index_s_plus_1  = -1
    var index_current   = -1
    var diff            = -1
    var top             = -1
    var first           = -1
    var last            = -1
    var reversed        = false
    var clear_no_blocks = false
    var rejectAfterNumMessages = ((1 << counterSize - 1) - (m-1)*n)
    println(rejectAfterNumMessages)
    def log2Upper(n: Int): Int = ceil(log(n) / log(2)).toInt

    def test_and_set_bit(addr: Int, block: Int): Boolean = {
        var old_val = this.memory(block)(addr)
        if(old_val == 1){
            this.resultArray.append(3)
            return true;
        }
        else{
            this.memory(block)(addr) = 1
            this.resultArray.append(4)
            return false
        }
    }

    //if method returns true, we drop the packet.
    def counter_validate(their_counter: Int): Boolean = {
        var index         :Int = -1;
        var i             :Int = -1;

        this.s_plus_1       = their_counter + 1
        this.index_s_plus_1 = s_plus_1 / n
        this.index_current  = this.counter / n
        this.diff           = index_s_plus_1 - index_current
        this.top            = if(diff >= this.m) m else diff
        this.first          = (this.counter%this.windowSizeValue) / n
        this.last           = (first + top) % (m)
        this.reversed       = (first > last)
        this.clear_no_blocks= (top == 0)
        
        //Wireguard check reject after # messages 
        if(this.s_plus_1 >= this.rejectAfterNumMessages){
            this.resultArray.append(2)
            return true;
        }

        //too old 
        if((this.s_plus_1 + n*(m-1)) < (this.counter)){
            this.resultArray.append(2)
            return true;
        }

        //new
        var newValueFlag:Boolean = false
        if((their_counter) > (this.counter)){
            if(!this.clear_no_blocks){
            for(i <- 1 to top)
                for(j <-0 until n)
                    memory((i+index_current)%(m))(j) = 0
            }
            this.counter = s_plus_1; 
            //println(this.counter)
            this.memory((their_counter/n) % m)(their_counter % n) = 1
            this.resultArray.append(1)
            return false;
            
        }
        //new or inside
        var ret :Boolean = test_and_set_bit(their_counter % n, (their_counter/n) % m)

        return ret
    }

        
    
}


object Main {
    def main(args: Array[String]){
        
    }
}
