import java.io.File;
import java.io.IOException;

import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.RefineryUtilities;
import org.moeaframework.Analyzer;
import org.moeaframework.Executor;
import org.moeaframework.Instrumenter;
import org.moeaframework.analysis.collector.Accumulator;
import org.moeaframework.core.NondominatedPopulation;
import org.moeaframework.core.Solution;


public class NSGAIIexample {

	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		//configure and run this experiment
		String problem = "ZDT1";
		String[] algorithms = { "NSGAIII", "NSGAII", "GDE3", "eMOEA" };
		
		NondominatedPopulation result = new Executor()
		.withProblem(problem)
		.withAlgorithm(algorithms[3])
		.withMaxEvaluations(10000)
		.distributeOnAllCores()
		.run();

		//display the results
		int COUNT = result.size();
		// In order to plot in jFreeChart
		// We need to store the objective values in a 2d array
		float [][] data = new float[2][COUNT];
		int i=0;		
		System.out.format("Objective1  Objective2%n");
		for (Solution solution : result) {
			System.out.format("%.4f      %.4f%n",
					solution.getObjective(0),
					solution.getObjective(1));
			data[0][i] = (float) solution.getObjective(0);
			data[1][i] = (float) solution.getObjective(1);
			i++;
		}

		// Instantiate a chart object of plotParetoFront
		plotParetoFront plotChart = new plotParetoFront("Pareto solutions", data);
		plotChart.pack();
		RefineryUtilities.centerFrameOnScreen(plotChart);
		plotChart.setVisible(true);


		

	}

}
