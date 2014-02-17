import scala.collection.mutable

object Problem005 {
  def main(args: Array[String]) {
    val n = 10
    val m = 12
    if (n > 100 || m > 100) throw new IllegalArgumentException
    //    val n = 3
    //    val m = 3
    val matrix = Array.ofDim[Boolean](n, m)

    val input =
      """
        |w........ww.
        |.www.....www
        |....ww...ww.
        |.........ww.
        |.........w..
        |..w......w..
        |.w.w.....ww.
        |w.w.w.....w.
        |.w.w......w.
        |..w.......w.
        |""".stripMargin.replaceAll("\n", "")
    //    val input =
    //      """
    //        |..w
    //        |.w.
    //        |www
    //        |""".stripMargin.replaceAll("\n", "")

    val mihoumon = new mutable.HashSet[(Int, Int)]
    for (i <- 0 until n; j <- 0 until m) {
      matrix(i)(j) = input.charAt(j + i * m) == 'w'
      mihoumon += new Tuple2(i, j)
    }

    println(soto(matrix, mihoumon, 0))
  }

  def soto(matrix: Array[Array[Boolean]], mihoumon: mutable.Set[(Int, Int)], count: Int): Int/*mutable.Set[(Int, Int)]*/ = {
    //    println("soto called")
    //    println(mihoumon.filter(e => matrix(e._1)(e._2)).head)
    val filteredMihoumon = mihoumon.filter(e => matrix(e._1)(e._2))
    if (filteredMihoumon.isEmpty) return count /*return new mutable.HashSet[(Int, Int)]*/
    val houmonzumi = naka(matrix, filteredMihoumon.head, filteredMihoumon)

    println("AA: " + houmonzumi)

    val sub = mihoumon &~ houmonzumi

    if (sub.isEmpty) return count/*return houmonzumi*/
    soto(matrix, sub, count+1)
  }

  def naka(matrix: Array[Array[Boolean]], t: (Int, Int), mihoumon: mutable.Set[(Int, Int)]): mutable.Set[(Int, Int)] = {
    val houmonzumi = new mutable.HashSet[(Int, Int)]
    if (!mihoumon.contains(t)) return houmonzumi

    houmonzumi += t
    if (matrix(t._1)(t._2)) {
      houmonzumi ++= naka(matrix, new Tuple2(t._1 - 1, t._2 - 1), mihoumon &~ houmonzumi)
      houmonzumi ++= naka(matrix, new Tuple2(t._1 - 1, t._2), mihoumon &~ houmonzumi)
      houmonzumi ++= naka(matrix, new Tuple2(t._1 - 1, t._2 + 1), mihoumon &~ houmonzumi)
      houmonzumi ++= naka(matrix, new Tuple2(t._1, t._2 - 1), mihoumon &~ houmonzumi)
      houmonzumi ++= naka(matrix, new Tuple2(t._1, t._2 + 1), mihoumon &~ houmonzumi)
      houmonzumi ++= naka(matrix, new Tuple2(t._1 + 1, t._2 - 1), mihoumon &~ houmonzumi)
      houmonzumi ++= naka(matrix, new Tuple2(t._1 + 1, t._2), mihoumon &~ houmonzumi)
      houmonzumi ++= naka(matrix, new Tuple2(t._1 + 1, t._2 + 1), mihoumon &~ houmonzumi)
    } else {
      houmonzumi
    }
    //    houmonzumi + t ++ (
    //      if (matrix(t._1)(t._2)) {
    //        naka(matrix, new Tuple2(t._1 - 1, t._2 - 1), mihoumon &~ houmonzumi) |
    //          naka(matrix, new Tuple2(t._1 - 1, t._2), mihoumon &~ houmonzumi) |
    //          naka(matrix, new Tuple2(t._1 - 1, t._2 + 1), mihoumon &~ houmonzumi) |
    //          naka(matrix, new Tuple2(t._1, t._2 - 1), mihoumon &~ houmonzumi) |
    //          naka(matrix, new Tuple2(t._1, t._2 + 1), mihoumon &~ houmonzumi) |
    //          naka(matrix, new Tuple2(t._1 + 1, t._2 - 1), mihoumon &~ houmonzumi) |
    //          naka(matrix, new Tuple2(t._1 + 1, t._2), mihoumon &~ houmonzumi) |
    //          naka(matrix, new Tuple2(t._1 + 1, t._2 + 1), mihoumon &~ houmonzumi)
    //      } else {
    //        houmonzumi
    //      }
    //    )
  }

}
