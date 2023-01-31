package corundum

import spinal.core._
import spinal.lib._
import scala.math.pow


object Quicky {
  def main(args: Array[String]) {

    def nextPowOf2(x: Int): Int = {
      var y = x - 1
      for (z <- 1 to 16) y = y | (y >> z)
      y + 1
    }
    
    for (x <- 1 to 64) {
      printf("x = % 3d, log2Up(% 3d)=% 3d, 1 << log2Up(% 3d)=% 3d, nextPowOf2(% 3d)=% 3d\n",
        x, x, log2Up(x), x, 1 << log2Up(x),x, nextPowOf2(x))
    }

    //       <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------> < L a  d  i  e  s
    println("01 02 03 04 05 06 01 02 03 04 05 06 01 02 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 4c 61 64 69 65 73".split(" ").mkString(""))
    //        a  n  d     G  e  n  t  l  e  m  e  n     o  f     t  h  e     c  l  a  s  s     o  f     '  9  9  :     I  f     I     c  o  u  l  d     o  f  f  e  r     y  o  u     o  n  l  y     o  n
    println("20 61 6e 64 20 47 65 6e 74 6c 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63 6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f 6e 6c 79 20 6f 6e".split(" ").mkString(""))
    //        e     t  i  p     f  o  r     t  h  e     f  u  t  u  r  e  ,     s  u  n  s  c  r  e  e  n     w  o  u  l  d     b  e     i  t  . <---------- Poly 1305 Tag (16 bytes) --------->
    println("65 20 74 69 70 20 66 6f 72 20 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69 74 2e 13 05 13 05 13 05 13 05 13 05 13 05 13 05 13 05 00 00 00 00".split(" ").mkString(""))


    printf("\n\n")
    //       <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------> < L a  d  i  e  s
    println("01 02 03 04 05 06 01 02 03 04 05 06 01 02 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 4c 61 64 69 65 73".split(" ").reverse.mkString(""))
    //        a  n  d     G  e  n  t  l  e  m  e  n     o  f     t  h  e     c  l  a  s  s     o  f     '  9  9  :     I  f     I     c  o  u  l  d     o  f  f  e  r     y  o  u     o  n  l  y     o  n
    println("20 61 6e 64 20 47 65 6e 74 6c 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63 6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f 6e 6c 79 20 6f 6e".split(" ").reverse.mkString(""))
    //        e     t  i  p     f  o  r     t  h  e     f  u  t  u  r  e  ,     s  u  n  s  c  r  e  e  n     w  o  u  l  d     b  e     i  t  . <---------- Poly 1305 Tag (16 bytes) --------->
    println("65 20 74 69 70 20 66 6f 72 20 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69 74 2e 13 05 13 05 13 05 13 05 13 05 13 05 13 05 13 05 00 00 00 00".split(" ").reverse.mkString(""))
    

    println("8B 72 92 DA 69 FB FA 82 12 67 A9 8C 5E A4 BE 3D D6 62 EE 36 A7 B5 E2 A9 FE 08 6E 29 51 ED AD A4 C2 7E EF 53 BC AF 86 7B DB 60 8E 64 34 8D 1A D3 07 06 05 04 03 02 01 00 01 00 00 00 90 00 00 04".split(" ").reverse.mkString(" "))
    println("4B C6 CE 86 65 D2 76 E5 9D 7A 4B 8E F0 DE F4 3F BC D7 31 48 8B 80 85 55 94 75 D6 FA E4 24 B3 FA 58 1B 09 28 E3 AE 03 98 8C 8B 77 2D 7F BD DD 92 36 3B CD 7E B6 A5 D6 05 29 0B 06 9E 0A DE 71 1A".split(" ").reverse.mkString(" "))
    println("00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 4E 8D CC 0A CA 55 87 50 01 E7 44 AF C1 AD 57 20 64 FF EF DD FB 63 76 A6 D7 6C 35 CE 03 0C 16 61".split(" ").reverse.mkString(" "))

    // !! 1. the Ladies and Gentlemen text is not there, the encrypted data is there. !!
    // !! 2. Make sure the Wireguard reserved field is 000000

    //       <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------>< L  a  d  i  e  s
    println("01 02 03 04 05 06 01 02 03 04 05 06 01 02 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 00 01 02 03 04 05 06 07 D3 1A 8D 34 64 8E".split(" ").reverse.mkString(""))
    //        a  n  d     G  e  n  t  l  e  m  e  n     o  f     t  h  e     c  l  a  s  s     o  f     '  9  9  :     I  f     I     c  o  u  l  d     o  f  f  e  r     y  o  u     o  n  l  y     o  n
    println("60 DB 7B 86 AF BC 53 EF 7E C2 A4 AD ED 51 29 6E 08 FE A9 E2 B5 A7 36 EE 62 D6 3D BE A4 5E 8C A9 67 12 82 FA FB 69 DA 92 72 8B 1A 71 DE 0A 9E 06 0B 29 05 D6 A5 B6 7E CD 3B 36 92 DD BD 7F 2D 77".split(" ").reverse.mkString(""))
    //       e     t  i  p     f  o  r     t  h  e     f  u  t  u  r  e  ,     s  u  n  s  c  r  e  e  n     w  o  u  l  d     b  e     i  t  . 
    println("8B 8C 98 03 AE E3 28 09 1B 58 FA B3 24 E4 FA D6 75 94 55 85 80 8B 48 31 D7 BC 3F F4 DE F0 8E 4B 7A 9D E5 76 D2 65 86 CE C6 4B 61 16 0C 03 CE 35 6C D7 A6 76 63 FB DD EF FF 64 20 57 AD C1 AF 44".split(" ").reverse.mkString(""))
    //       ...Poly 1305 Tag (16 bytes)->
    println("E7 01 50 87 55 CA 0A CC 8D 4E 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""))
    // 8E64348D1AD30706050403020100010000000000000400007201b315b31500000000000000000000118877665544332211450201060504030201060504030201
    // 772D7FBDDD92363BCD7EB6A5D605290B069E0ADE711A8B7292DA69FBFA821267A98C5EA4BE3DD662EE36A7B5E2A9FE086E2951EDADA4C27EEF53BCAF867BDB60
    // 44AFC1AD572064FFEFDDFB6376A6D76C35CE030C16614BC6CE8665D276E59D7A4B8EF0DEF43FBCD731488B8085559475D6FAE424B3FA581B0928E3AE03988C8B
    // 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004E8DCC0ACA55875001E7
  }
}