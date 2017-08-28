package in.edu.insofe.ngram;

import java.io.IOException;
import java.util.regex.Pattern;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

public class NGramMapper extends Mapper<LongWritable,Text,Text,NullWritable>{
	private String GRAM_LENGTH="ngrams_length";
	private int gram_length=0;
	private Pattern space_pattern=Pattern.compile("[ ]");
	StringBuilder grambuilder=new StringBuilder();
	public void setup(Context context) throws IOException,InterruptedException{
		gram_length=context.getConfiguration().getInt(GRAM_LENGTH,0);
	}
	
	public void map(LongWritable key,Text value,Context context) throws IOException,InterruptedException{
		String tokens[]=space_pattern.split(value.toString());
		String strToken=null;
		
		for(int i=0;i<tokens.length;i++){
			strToken=tokens[i];
			grambuilder.setLength(0);
			if(i+gram_length <= tokens.length){
				for(int j=i;j< i+gram_length;j++){
					grambuilder.append(tokens[j]);
					grambuilder.append(" ");
				}
				context.write(new Text(grambuilder.toString()), NullWritable.get());
			}
		}
	}
}
/*A MapReduce job usually splits the
  when i=0;
  gram_length=2
  if(2 <=6){
  	 for(j=0;j<2;j++){
  	 	A" "MapReduce
  	 }	
  }
  
  when i=1;
  gram_length=2
  if(3 <=6){
  	 for(j=1;j<3;j++){
  	 	MapReduce" "job
  	 }	
  }
  		
 */


