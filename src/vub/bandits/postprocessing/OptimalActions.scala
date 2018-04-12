package vub.bandits.postprocessing

import org.jfree.chart.ChartFactory
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.DefaultXYDataset
import vub.bandits.algorithms.Report.RunReport

object OptimalActions {
  def bestArmFn(arm: Int): (Int => Boolean) =
    (a: Int) => arm == a

  def percOptimal(bestArms: Vector[Int=>Boolean], experiment: Vector[RunReport], steps: Int): Array[Double] = {
    val optimalActions = Array.fill(steps)(0.0)
    for (i <- 0 to steps - 1) {
      for (j <- 0 to experiment.length - 1) {
        val arm = experiment(j).steps(i).arm
        val isOptimal = bestArms(j)(arm)
        optimalActions(i) += (if (isOptimal) 1 else 0)
      }
    }
    for (i <- 0 to steps - 1) {
      optimalActions(i) /= experiment.length
      optimalActions(i) *= 100
    }
    optimalActions
  }

  def plot(bestArms: Vector[Int=>Boolean], experiments: Map[String, Vector[RunReport]], steps: Int) = {
    val x = Array.range(0, steps).map(_.toDouble)

    val dataset = new DefaultXYDataset
    for (algo <- experiments.keys) {
      val y = percOptimal(bestArms, experiments(algo), steps)
      dataset.addSeries(algo, Array(y,x))
    }

    val renderer = new XYLineAndShapeRenderer(true, false)

    val chart = ChartFactory.createScatterPlot(
      null,
      "Optimal actions (%)",
      "Iterations",
      dataset,
      org.jfree.chart.plot.PlotOrientation.HORIZONTAL,
      true,false,false
    )
    chart.getXYPlot.setRenderer(0, renderer)
    chart
  }
}
