package vub.bandits.algorithms

import scala.collection.mutable.ListBuffer

class Rewards {
  private var rewards = new ListBuffer[Double]()

  def add(reward: Double) = rewards += reward
  def mean = rewards.sum / rewards.length
}
