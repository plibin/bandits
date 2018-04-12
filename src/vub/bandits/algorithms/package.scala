package vub.bandits

import scala.collection.mutable.ListBuffer

package object algorithms {
  def sampleMean(rewards: ListBuffer[Double]) =
    rewards.sum / rewards.length

  def sumOfSquares(rewards: ListBuffer[Double], sampleMean: Double) =
    rewards.map(r => (r - sampleMean) * (r - sampleMean)).sum

  def maxIndex[T](sequence: IndexedSeq[T], f: (T) => Double): Int = {
    var maxIndex = 0
    var maxValue = f(sequence(0))
    for (i <- 1 to (sequence.length - 1)) {
      val value = f(sequence(i))
      if (value > maxValue) {
        maxIndex = i
        maxValue = value
      }
    }
    maxIndex
  }
}
