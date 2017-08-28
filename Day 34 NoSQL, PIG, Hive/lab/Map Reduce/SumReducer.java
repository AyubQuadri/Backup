package in.edu.insofe.wc;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

public class SumReducer extends Reducer<Text,IntWritable,Text,IntWritable>{
		public void reduce(Text key,Iterable<IntWritable> values,Context context) throws IOException,InterruptedException{
		    // initialize the sum for each keyword
			int wordcount=0;
			for(IntWritable value:values){
				wordcount+=value.get();
			}
			// create a pair <keyword, number of occurences>
			context.write(key, new IntWritable(wordcount));
		}

}


