package in.edu.insofe.ngram;
/*
 * 1Gram
 * This is a hadoop class
 * this
 * is
 * a 
 * hadoop
 * class
 * 
 * This is
 * is a
 * a hadoop
 * hadoop class
 * 
 * 
 * 
 */
import java.io.IOException;

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

public class NGramJob extends Configured implements Tool{

	/**
	 * @param args
	 */
	private String GRAM_LENGTH="ngrams_length";
	
	public static void main(String[] args) throws Exception {
		int exitcode=ToolRunner.run(new NGramJob(), args);
		System.exit(exitcode);
	}

		
	private String deletePath(String strPath) throws IOException{
		FileSystem fs=FileSystem.get(getConf());
		fs.delete(new Path(strPath),true);
		return strPath;
	}
	@Override
	public int run(String[] args) throws Exception {
		if(args.length!=3){
			System.out.printf("Usage %s: [generic options] <input dir> <output dir>", this.getClass().getName());
			ToolRunner.printGenericCommandUsage(System.out);
			return 1;
		}
		getConf().setInt(GRAM_LENGTH, Integer.parseInt(args[2]));
		Job job=Job.getInstance(getConf(),"NGram");
		job.setJarByClass(NGramJob.class);
		job.setInputFormatClass(TextInputFormat.class);
		job.setOutputFormatClass(TextOutputFormat.class);
		job.setMapperClass(NGramMapper.class);
		job.setNumReduceTasks(0);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(NullWritable.class);
		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(deletePath(args[1])));
		
		if(job.waitForCompletion(true)){
			return 1;
		}
		return 0;
	}

}
