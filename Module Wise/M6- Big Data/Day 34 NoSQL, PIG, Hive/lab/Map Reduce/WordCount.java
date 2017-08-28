package in.edu.insofe.wc;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class WordCount{
	// Driver program
	public static void main(String args[]) throws Exception{
	    Configuration conf=new Configuration();
		// create a job with name "WordCount"
		Job job=Job.getInstance(conf, "WordCount");
		//Set the Jar by finding where a given class came from.
		job.setJarByClass(WordCount.class);
		
		//We will give 2 arguments at the run time, one in input path and other is output path
		// //set the HDFS path of the input data to be fetched from the command line
		FileInputFormat.addInputPath(job, new Path(args[0]));
		//  // set the HDFS path for the output
		FileOutputFormat.setOutputPath(job, new Path(args[1]));
		
		// https://hadoop.apache.org/docs/r1.0.4/api/org/apache/hadoop/mapreduce/Job.html
		//Providing the mapper and reducer class names
		job.setMapperClass(WordMapper.class);
		job.setReducerClass(SumReducer.class);
		
		//Setting configuration object with the Data Type of Mapper Key and Value
		// Set the key class for the map output data.
		job.setMapOutputKeyClass(Text.class);
		// Set the value class for the map output data.
		job.setMapOutputValueClass(IntWritable.class);
		
		//Setting configuration object with the Data Type of output Key and Value
		// Set the key class for the job output data.
		job.setOutputKeyClass(Text.class);
		// Set the value class for job outputs.
		job.setOutputValueClass(IntWritable.class);
		
		// Submit the job to the cluster and wait for it to finish
		job.waitForCompletion(true);
	}

}
