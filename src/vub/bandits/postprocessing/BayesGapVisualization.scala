package vub.bandits.postprocessing

import java.awt.{BasicStroke, Color}

import org.jfree.chart.ChartFactory
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.data.xy.{YIntervalSeries, YIntervalSeriesCollection}
import org.jfree.ui.RectangleInsets
import vub.bandits.Bandit
import vub.bandits.algorithms.BayesGap.Bounds
import vub.bandits.algorithms.UCB.{Maximizing, UpperBound}
import vub.bandits.algorithms.posteriors.Posterior
import vub.bandits.rand.RNG

object BayesGapVisualization {
  def plot(dataset: YIntervalSeriesCollection) = {
    val chart = ChartFactory.createTimeSeriesChart(
      "BayesGap", // chart title
      "Iteration", // x axis label
      "Posterior", // y axis label
      dataset, // data
      true, // include legend
      true, // tooltips
      false // urls
    );

    // get a reference to the plot for further customisation...
    val plot = chart.getXYPlot();
    plot.setDomainPannable(true);
    plot.setRangePannable(false);
    plot.setInsets(new RectangleInsets(5, 5, 5, 20));

    val renderer = new DeviationRenderer(true, false);
    renderer.setSeriesStroke(0, new BasicStroke(3.0f, BasicStroke.CAP_ROUND,
      BasicStroke.JOIN_ROUND));
    renderer.setSeriesStroke(0, new BasicStroke(3.0f,
      BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
    renderer.setSeriesStroke(1, new BasicStroke(3.0f,
      BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
    renderer.setSeriesFillPaint(0, new Color(255, 200, 200));
    renderer.setSeriesFillPaint(1, new Color(200, 200, 255));
    plot.setRenderer(renderer);

    // change the auto tick unit selection to integer units only...
    val yAxis = (plot.getRangeAxis());
    //yAxis.setAutoRangeIncludesZero(false);
    yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());

    //plot.getDomainAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())

    Plot.show("Posteriors", chart)
  }
}
