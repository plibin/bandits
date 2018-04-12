package vub.bandits.postprocessing

import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.data.function.NormalDistributionFunction2D
import org.jfree.data.general.DatasetUtilities
import org.jfree.data.xy.{XYDataset, XYSeriesCollection}
import vub.bandits.algorithms.posteriors.Posterior
import org.jfree.data.function.Function2D

/**
  * Created by plibin on 17/08/17.
  */
object Posteriors {
  def plot(posteriors: Vector[Posterior]) = {
    val ds = createDataset(posteriors)
    createChart(ds)
  }

  def createDataset(posteriors: Vector[Posterior]) : XYDataset = {
    val dataset = new XYSeriesCollection()

    for (i <- 0 to posteriors.length - 1) {
      val p = posteriors(i)
      val f = new Function2D {
        override def getValue(x: Double): Double = p.pdf(x)
      }
      val series = DatasetUtilities.sampleFunction2DToSeries(f, 0, 1, 1000, "arn" + i)
      dataset.addSeries(series)
    }

    dataset;
  }

  def createChart(dataset: XYDataset): JFreeChart = {
    val chart = ChartFactory.createXYLineChart(
      "Posteriors",
      "X",
      "Y",
      dataset,
      PlotOrientation.VERTICAL,
      true,
      true,
      false
    );
    chart
  }
}
