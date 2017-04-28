package leetcode

object StudentAttendanceRecordII {
  def run(n: Int): Int = {
    lazy val counts: Stream[(Int, Int)] = (1, 0) #:: (2, 1) #:: (4, 4) #:: counts.zip(counts.tail).zip(counts.drop(2)).map {
      case (((m3, m3a), (m2, m2a)), (m1, m1a)) =>
        val m0 = m1 + m2 + m3
        val m0a = m1a + m1 + m2a + m2 + m3a + m3
        (m0, m0a)
    }

    counts(n)._1 + counts(n)._2
  }
}
