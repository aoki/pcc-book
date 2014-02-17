object Problem006 {
  def main(args: Array[String]) {
    val input =
      """#S##
        |#.#G
        |..#.
        |#...
        |""".stripMargin.split("\n").toList.map( l => l.toList )
    val x = for (i <- 0 to input.size - 1; j <- 0 to input(0).size - 1; if input(i)(j) == 'S') yield (i, j)
    println(x)

  }
}
