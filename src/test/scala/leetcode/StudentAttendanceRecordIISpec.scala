package leetcode

import org.scalacheck._

object StudentAttendanceRecordIISpec extends Properties("StudentAttendanceRecordII") {
  val smallInt = Gen.choose(0, 10)
  
  property("base") = Prop.forAll(smallInt) { n =>
    println("n = " + n)
    val hasA = (0 to (n - 1))
      .map(p => Seq.fill(p)("P") ++ Seq.fill(n - 1 - p)("L") :+ "A")
      .flatMap(_.permutations)

    val noA = (0 to n)
      .map(p => Seq.fill(p)("P") ++ Seq.fill(n - p)("L"))
      .flatMap(_.permutations)

    val ans = (hasA ++ noA).filterNot { list =>
      list.sliding(3).exists(_ == Seq("L", "L", "L"))
    }
    
    ans.size == StudentAttendanceRecordII.run(n)
  }
}
