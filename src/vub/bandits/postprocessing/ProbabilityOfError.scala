package vub.bandits.postprocessing

import org.jfree.chart.ChartFactory
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.DefaultXYDataset
import vub.bandits.algorithms.Report.RunReport
import vub.bandits.algorithms.Rewards

object ProbabilityOfError {
  def selectBestArm(rewardsPerArm: Vector[Rewards]): Int = {
    val r: Rewards = rewardsPerArm.maxBy(_.mean)
    rewardsPerArm.indexOf(r)
  }

  def probOfError(bestArm: Int, nrArms: Int, experiment: Vector[(RunReport, Int)], steps: Int) = {
    val errorPerStep = Array.fill(steps)(0.0)

    def rewardsPerArm = Vector.fill(nrArms)(new Rewards)
    val rewardsPerExp = Vector.fill(experiment.length)(rewardsPerArm)
    for (i <- 0 to steps - 1) {
      var errors = 0
      for (j <- 0 to experiment.length - 1) {
        val step = experiment(j)._1.steps(i)
        rewardsPerExp(j)(step.arm).add(step.reward)

        val error = selectBestArm(rewardsPerExp(j)) != bestArm
        errors += (if (error) 1 else 0)
      }

      errorPerStep(i) = errors.toDouble / experiment.length
    }

    errorPerStep
  }

  def plot(bestArm: Int, nrArms: Int, experiments: Map[String, Vector[(RunReport, Int)]], steps: Int) = {
    val x = Array.range(0, steps).map(_.toDouble)

    val dataset = new DefaultXYDataset
    for (algo <- experiments.keys) {
      val y = probOfError(bestArm, nrArms, experiments(algo), steps)
      dataset.addSeries(algo, Array(y,x))
    }

    val renderer = new XYLineAndShapeRenderer(true, false)

    val chart = ChartFactory.createScatterPlot(
      null,
      "Probability of error",
      "Iterations",
      dataset,
      org.jfree.chart.plot.PlotOrientation.HORIZONTAL,
      true,false,false
    )
    chart.getXYPlot.setRenderer(0, renderer)
    chart
  }
}
