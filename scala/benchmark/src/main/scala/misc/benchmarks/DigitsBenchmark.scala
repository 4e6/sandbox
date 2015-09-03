package misc.benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{ Benchmark, OutputTimeUnit, Scope, State }

import misc.utils.NumericUtils

@State(Scope.Thread)
class DigitsBenchmark {

  private val x: Int = {
    val coeffs = 0 until 9 map { _ ⇒ util.Random.nextInt(10) }
    NumericUtils.poly(coeffs).toInt
  }

  @Benchmark
  def digitsNaive =
    NumericUtils.digitsNaive(x)

  @Benchmark
  def digitsInt =
    NumericUtils.digitsInt(x)

  @Benchmark
  def digitsT =
    NumericUtils.digitsT(x)

  @Benchmark
  def digits =
    NumericUtils.digits(x)
}
