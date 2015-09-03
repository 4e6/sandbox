package misc.benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{ Benchmark, OutputTimeUnit }

import misc.utils.NumericUtils

class IsEvenBenchmark {
  import IsEvenBenchmark.x

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def isEvenInt(): Unit =
    NumericUtils.isEvenInt(x)

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def isEvenT(): Unit =
    NumericUtils.isEvenT(x)

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def isEven(): Unit =
    NumericUtils.isEven(x)

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def isOddT(): Unit =
    NumericUtils.isOddT(x)

  @Benchmark
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def isOdd(): Unit =
    NumericUtils.isOdd(x)
}

object IsEvenBenchmark {
  final val x: Int = {
    val coeffs = 0 until 9 map { _ ⇒ util.Random.nextInt(10) }
    NumericUtils.poly(coeffs).toInt
  }
}
