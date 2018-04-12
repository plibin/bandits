package vub.bandits

package object utils {
  def normalize(x: Double, min: Double, max: Double): Double = {
    if (x < min) 0.0
    else if (x > max) 1.0
    else {
      val normalized = (x - min) / (max - min)
      normalized
    }
  }

  def maxIndex[T](v: Seq[T], f: T => Double): Int =
    v.zipWithIndex.maxBy{case (datum, index) => f(datum)}._2
  def minIndex[T](v: Seq[T], f: T => Double): Int =
    v.zipWithIndex.minBy{case (datum, index) => f(datum)}._2

  def square(d: Double) = d * d

  def sum(s: Seq[Double]) = s.foldLeft(0.0)(_ + _)
  def mean(s: Seq[Double]) = sum(s) / s.length
}

