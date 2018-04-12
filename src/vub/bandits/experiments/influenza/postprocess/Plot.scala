package vub.bandits.experiments.influenza.postprocess

import java.io.File

import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.postprocessing.{AverageReward, OptimalActions}

import scala.io.Source

import scala.collection.breakOut

/**
  * Tool to postprocess the result of a batch influenza bandit run,
  * where each bandit evaluation was serialized to a CSV file.
  *
  * This is a difficult program to parameterize via the command line,
  * therefore, we just have one particular use case that can be easily adapted.
  */
object Plot extends App {
  val r0 = "r0_14"
  val steps = 1000
  val nrBandits = 500

  val base = "~/one-100/" + r0 + "/"
  val results = Map(
    "eps-greedy" -> new File(base, "eps"),
    "Bayes-UCB" -> new File(base, "ucb")
  )

  val output_dir = "~/tmp/UAI/"

  val experiments = parseExperiments(steps, nrBandits, results)
  val avg = AverageReward.plot(experiments, steps)
  vub.bandits.postprocessing.Plot.exportToPDF(
    avg,
    new File(output_dir, "avg_reward_" + r0 + ".pdf"),
    500, 300)

  val optimal = OptimalActions.plot(Vector.fill(nrBandits){arm => arm==8}, experiments, steps)
  vub.bandits.postprocessing.Plot.exportToPDF(
    optimal,
    new File(output_dir, "optimal_actions_" + r0 + ".pdf"),
    500, 300)


  def parseExperiments(steps: Int, nrBandits: Int, results: Map[String, File]) = {
    val experiments: Map[String, Vector[RunReport]] = for {
      (experimentName,experimentDir) <- results
    } yield (experimentName, experiment(experimentDir, nrBandits))
    experiments
  }

  def isRunCsv(f: File) =
    f.getName.startsWith("run-") && f.getName.endsWith(".csv")

  def experiment(resultsDir: File, nrBandits: Int): Vector[RunReport] = {
    val runReport = for {
      i <- 1 to nrBandits
      f = new File(resultsDir, "run-" + i + ".csv")
    } yield csvToReport(f)

    runReport.toVector
  }

  def csvToReport(csv: File): RunReport = {
    println(csv.getAbsolutePath)

    val lines = Source.fromFile(csv).getLines()

    val headers = lines.next().split(",")
    val values = lines.next().split(",")

    val namedValues: Map[String, String] = (headers zip values)(breakOut)

    // -1: ignore the algorithm column
    // /2: each arm + reward share an index
    val stepsReports = for {
      i <- 0 until (headers.length - 1)/2
      arm = Integer.parseInt(namedValues("arm_"+i), 2)
      reward = namedValues("reward_"+i).toDouble
    } yield new StepReport(arm, reward)

    new RunReport(stepsReports.toVector)
  }
}
