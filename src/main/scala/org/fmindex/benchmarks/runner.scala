package org.fmindex.benchmarks
import com.google.caliper.{Runner => CaliperRunner}




object BenchmarkRunner {
  def main(args: Array[String]) {
    CaliperRunner.main(classOf[Benchmark], args: _*)
  }
}

object BenchmarkBWTWriteRunner {
  def main(args: Array[String]) {
    CaliperRunner.main(classOf[Benchmark], args: _*)
  }
}

