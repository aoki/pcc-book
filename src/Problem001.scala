object Problem001 extends App {
  println(compute(4, 15, List(1, 20, 12)))

  def compute(d: Int, m: Int, l: List[Int]): Boolean = {
    assert(preCheck(m, l))
    existCertainSum(d, m, l, 0)
  }

  def preCheck(m: Int, l: List[Int]): Boolean = {
    val n = l.size
    val upperBound = Math.pow(10, 8).toInt
    assert(n > 0 && n <= 50, "n: Domain error. Must be (0 < n <= 50).")
    assert(m > 0 && m <= upperBound, s"m: Domain error. Must be (0 < m <= $upperBound).")

    l.foreach(k =>
      assert(k > 0 && k <= upperBound, s"k: Domain error. Must be (0 < k <= $upperBound}). $k has been detected.")
    )
    true
  }

  /**
   * 与えられた数字のリストから、d回数字を選び、その総和がmと等しくなるような組み合わせは存在するか調べる関数（部分和問題）
   * @param d 選ぶ回数
   * @param m 目標の値
   * @param l 整数のリスト
   * @param sum 総和
   * @return
   */
  def existCertainSum(d: Int, m: Int, l: List[Int], sum: Int): Boolean = {
    if (sum > m) return false
    if (d == 0 && sum == m) return true
    if (d == 0) return false

    for (k <- l) if (existCertainSum(d - 1, m, l, sum + k)) return true
    false
  }
}
