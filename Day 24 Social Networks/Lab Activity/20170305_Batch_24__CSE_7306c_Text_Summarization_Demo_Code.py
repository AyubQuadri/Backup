
text3='''The novel algorithm called bigram topical item response theory (BIRT) for sentiment classification 
is achieved by an objective function which builds the model for the representation and then predicts the document 
sentiment. The design of adaptive text paper and education assessment based on IRT (item response theory) is proposed 
[18, 19].
The topical sentiment analysis recognizes the polarity of opinion and emotion attributes regarding the topic of
interest. The subjectivity strongly depends on its sentences or messages. The novelty of lexicon and its semantics 
can be used to distinguish between words and phrases in determining the polarity of the sentiment [20, 21].
The modified item response theory helps to achieve personalized learning and provide learning pathways and helps 
them to learn effectively [22, 23]. The novelty of the proposed method is that the students' tweets are not simply 
classified by sentiment polarity but instead generate the grading of sentiment for each selected topic. In this paper
rather than using the opinion polarities of each message relevant to the topic, the sentence level opinion 
classification based on BIRT is discussed. Unlike the fixed set of responses, dynamic response theory in terms of 
multiple factors on varied topics by different sets of interactions between the user communities offers the novelty
of sentiment analysis.

In the previous study, the problem of classification deals with traditional statistical and probability distribution 
for scaling the polarity. The mapping from the real line is provided through the probability interval 0 to 1. 
Using such logit functional model the real time traffic crash can be predicted using the traffic-flow variables and 
rain data . 
The bias component can be incorporated into the classification with the help of the IRT model which in turn generalizes
the data and minimizes cost of parameter estimation. The likelihood of the responses and item level analysis can be 
formulated through the IRT model.'''




import sumy
from sumy.parsers.plaintext import PlaintextParser #We're choosing a plaintext parser here, other parsers available for HTML etc.
from sumy.nlp.tokenizers import Tokenizer 
from sumy.summarizers.lex_rank import LexRankSummarizer #We're choosing Lexrank, other algorithms are also built in


#file = "plain_text.txt" #name of the plain-text file
#parser = PlaintextParser.from_file(file, Tokenizer("english"))

parser = PlaintextParser(text3, Tokenizer("english"))
summarizer = LexRankSummarizer()

summary = summarizer(parser.document, 3) #Summarize the document with 3 sentences

for sentence in summary:
    print sentence




