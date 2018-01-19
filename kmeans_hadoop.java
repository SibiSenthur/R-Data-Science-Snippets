import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;


public class KMeans {

	public static enum Counters {
		CENTROIDS
	};

	public static class KMeansMapper
	extends Mapper<Object, Text, Text, Text> {
		ArrayList<ArrayList<Double>> centroids = new ArrayList<ArrayList<Double>>();

		protected void setup(Context context) throws IOException {
			/*
			 * This method is for reading centroids before running map()
			 * 
			 * setup() and cleanup() are methods to initialize and clean up your map/reduce tasks
			 * setup -> map -> cleanup OR setup -> reduce -> cleanup
			 */
			Configuration conf = context.getConfiguration();
			// conf.get is related to line 194 in Driver.
			BufferedReader reader = new BufferedReader(new FileReader(
					new File(conf.get("centroidPath"), "part-r-00000")));  
			String line;
			while ((line = reader.readLine()) != null)
				centroids.add(readPoint(line));
			reader.close();
		}

		public void map(Object key, Text value, Context context
				) throws IOException, InterruptedException {
			/*
			 * For algorithm please check page 61 in slides.
			 */
			ArrayList<Double> point = readPoint(value.toString());
			context.write(new Text(String.valueOf(getCentroidId(centroids, point))), 
					new Text(value.toString()));
		}
	}

	public static class KMeansReducer
	extends Reducer<Text, Text, NullWritable, Text> {

		public void reduce(Text key, Iterable<Text> values,
				Context context
				) throws IOException, InterruptedException {

			/*
			 * For algorithm please check page 61 in slides.
			 */

			int counter = 0;
			int n = 0;
			ArrayList<Double> centroid = new ArrayList<Double>();
			for (Text val : values) {
				String[] pointStr = val.toString().split(",");
				n = pointStr.length;
				if (counter == 0) {
					for (String num : pointStr)
						centroid.add(Double.parseDouble(num));
				} else {
					for (int i = 0; i < n; i++)
						centroid.set(i, 
								centroid.get(i) + Double.parseDouble(pointStr[i]));
				}
				counter++;
			}
			String centroidStr = "";
			for (int i = 0; i < n; i++)
				centroidStr += "," + String.valueOf(centroid.get(i) / counter);
			context.write(NullWritable.get(), new Text(centroidStr.substring(1)));
		}
	}

	public static class ReportCentroids
	extends Mapper<Object, Text, Text, Text> {
		ArrayList<ArrayList<Double>> dictionary = new ArrayList<ArrayList<Double>>();
		int topN = 0;

		protected void setup(Context context) throws IOException {
			Configuration conf = context.getConfiguration();
			BufferedReader reader = new BufferedReader(new FileReader(
					new File(conf.get("dictionaryFile"))));
			String line;
			while ((line = reader.readLine()) != null)
				dictionary.add(readPoint(line));
			reader.close();
			topN = conf.getInt("topN", 0)
		}

		public void map(Object key, Text value, Context context
				) throws IOException, InterruptedException {

			ArrayList<Double> centroid = readPoint(value.toString());
			ArrayList<Double> sortedCentroid = new ArrayList<>(centroid);
			Collections.sort(sortedCentroid);
			topNWords = "";
			for (int i = sortedCentroid.size()-1; i >= sortedCentroid.size()-topN; i--)
				topNWords += "," + dictionary.get(centroid.indexOf(sortedCentroid.get(i)));
				context.getCounter(Counters.CENTROIDS).increment(1);
			context.write(new Text(context.getCounters().findCounter(Counters.CENTROIDS).getValue().toString())), 
					new Text(topNWords.substring(1)));
		}
	}

	public static double getEuclideanDist(ArrayList<Double> p1, ArrayList<Double> p2) {
		double dist = 0;
		for(int i = 0; i < p1.size(); i++)
			dist += Math.pow(p1.get(i) - p2.get(i), 2);
		return Math.sqrt(dist);
	}

	public static ArrayList<Double> readPoint(String line) {
		ArrayList<Double> point = new ArrayList<Double>();
		for (String num : line.trim().split(","))
			point.add(Double.parseDouble(num));
		return point;
	}

	public static int getCentroidId(ArrayList<ArrayList<Double>> centroids, ArrayList<Double> point) {
		/*
		 * For given centroids and a certain point
		 * return which the point belongs to in terms of centroid id.
		 */
		int centroidId = -1;
		double minDist = Double.POSITIVE_INFINITY;
		for (int i = 0; i < centroids.size(); i++) {
			double curDist = getEuclideanDist(centroids.get(i), point);
			if (curDist < minDist) {
				centroidId = i;
				minDist = curDist;
			}
		}
		return centroidId;
	}

	public static void main(String[] args) throws Exception {		

		String inputPath = args[0];
		String outputPath = args[1];
		String centroidPath = args[2];
		int numCentroids = Integer.parseInt(args[3]);
		int maxIter = Integer.parseInt(args[4]);
		String dictionaryFile = args[5];
		String reportTopN = args[6]

//		String inputPath = "input_kmeans.txt";
//		String centroidPath = "centroids/";
//		int numCentroids = 4;
//		int maxIter = 10;
//		String outputPath = "output";

		// Job for initializing centroids
		Configuration conf0 = new Configuration();
		conf0.setInt("maxIter", numCentroids);
		Job job0 = Job.getInstance(conf0, "Centroids Initialization");
		job0.setMapperClass(CentroidsInitializer.class);
		job0.setOutputKeyClass(NullWritable.class);
		job0.setOutputValueClass(Text.class);
		FileInputFormat.addInputPath(job0, new Path(inputPath));
		FileOutputFormat.setOutputPath(job0, new Path(centroidPath + "0"));
		job0.waitForCompletion(true);

		// find centroids of clusters after maxIter times
		for (int i = 0; i < maxIter; i++) {
			Configuration conf = new Configuration();
			conf.set("centroidPath", centroidPath + String.valueOf(i));
			Job job = Job.getInstance(conf, "KmeansClustering" + String.valueOf(i));
			job.setJarByClass(KMeans.class);
			job.setMapperClass(KMeansMapper.class);
			job.setReducerClass(KMeansReducer.class);
			job.setOutputKeyClass(Text.class);
			job.setOutputValueClass(Text.class);
			FileInputFormat.addInputPath(job, new Path(inputPath));
			FileOutputFormat.setOutputPath(job, new Path(centroidPath + String.valueOf(i + 1)));
			job.waitForCompletion(true);
		}

		// find membership for each cluster
		Configuration conf = new Configuration();
		conf.set("dictionaryFile", dictionaryFile);
		conf.setInt("topN", reportTopN)
		Job job = Job.getInstance(conf, "Report Centroids");
		job.setJarByClass(KMeans.class);
		job.setMapperClass(ReportCentroids.class);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(Text.class);
		FileInputFormat.addInputPath(job, new Path(centroidPath + String.valueOf(maxIter)));
		FileOutputFormat.setOutputPath(job, new Path(outputPath));
		job.waitForCompletion(true);
	}
}