package in.edu.insofe.wc;

import java.io.IOException;
import java.util.StringTokenizer;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

public class WordMapper extends Mapper<LongWritable,Text,Text,IntWritable>{
     // type of output key
	private Text word = new Text();
	//map method that performs the tokenize job and framing the initial key value pairs
	public void map(LongWritable key,Text value,Context context) throws IOException,InterruptedException{
		//taking one line at a time and tokenizing the same
		String line = value.toString();
        StringTokenizer tokenizer = new StringTokenizer(line);
		
		//iterating through all the words available in that line and forming the key value pair
        while (tokenizer.hasMoreTokens()) {
            word.set(tokenizer.nextToken());
			//sending to output collector which in turn passes the same to reducer
            context.write(word, new IntWritable(1));
        }
	}
}

