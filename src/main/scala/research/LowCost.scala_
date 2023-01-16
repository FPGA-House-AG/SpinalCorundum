// Research
// Modeling the architecture as proposed in Low-Cost and Programmable CRC Implementation based on FPGA (Extended Version)

package research

// Breeze is a linear algebra library for Scala
import breeze.linalg.{DenseMatrix, DenseVector}

// a DenseVector is a column vector; for a row vector use a 1-row matrix.
// Transposing column into a row vector using the transpose method .t

object LowCost {
  def main(args: Array[String]) {
    val el = 32
    val polynomial = 0x04C11DB7
    // vector_S is a column vector with the polynomial bits
    // first vector item is the most-significant bit of polynomial
    val vector_S = DenseVector.zeros[Int](el)
    // initialize vector_S from polynomial
    for (i <- 0 until el) {
        if ((polynomial & (1 << (el - i - 1))) != 0) vector_S(i) = 1
    }
    println(vector_S)

    val matrix_T = DenseMatrix.zeros[Int](el, el)

    // set first column to polynomial G
    //for (i <- 0 until el) {
    //    matrix_T(i, 0) = vector_S(i)
    //}
    // set first column to polynomial G
    matrix_T(::, 0) := vector_S

    // add shift operation of the LFSR into matrix T (diagonal of 1 bits)
    //for (i <- 0 until el - 1) {
    //    matrix_T(i, i + 1) = 1
    // }
    // add shift operation of the LFSR into matrix T (diagonal of 1 bits)
    matrix_T(0 until el - 1, 1 until el) := DenseMatrix.eye[Int](el - 1)
    //println(matrix_T)

    val n = 512

    // matrices_Ti with i in [1, n] is T to the power of i, multiplication in GF(2), i.e. mod 2
    // matrices_Ti with i == 0 is the identity matrix
    var matrices_Ti = Vector.fill(n + 1)(DenseMatrix.zeros[Int](el, el))
    matrices_Ti(0) := DenseMatrix.eye[Int](el)
    matrices_Ti(1) := matrix_T
    for (ni <- 2 to n/*including*/) {
        matrices_Ti(ni) := matrices_Ti(ni - 1) * matrix_T
        matrices_Ti(ni) := matrices_Ti(ni).mapValues(x => x % 2)
    }

    // See paper (5)
    // Ti * S results in column vector with height el [el * el] * [ el * 1 ] = [ el * 1 ]
    // Weln = [Tn-1 * S, Tn-2 * S, ..., T * S, S] so n columns, each column of height el

    // column matrix of S [el * 1]
    val matrix_S = vector_S.toDenseMatrix.t
    var matrix_Weln = matrix_S
    println(matrix_Weln)
    for (ni <- 1 until n) {
      val matrix_TiS = matrices_Ti(ni) * matrix_S
      val matrix_TiSmod2 = matrix_TiS.mapValues(x => x % 2)
      // prefix column TiS to the existing Weln matrix
      matrix_Weln = DenseMatrix.horzcat(matrix_TiSmod2, matrix_Weln)
    }
    println(matrix_Weln(::, n - 8 until n))

  }
}