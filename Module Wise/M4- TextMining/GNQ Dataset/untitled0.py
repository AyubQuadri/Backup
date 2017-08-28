# -*- coding: utf-8 -*-
"""
Created on Sat Mar 25 10:10:33 2017

@author: quadris
"""

import os 
import pandas as pd
import nltk, string
from sklearn.feature_extraction.text import TfidfVectorizer

os.chdir("C:\\Users\\quadris\\Desktop\\My Learning\\INSOFE CPEE\\Module Wise\\M4- TextMining\\GNQ Dataset")

rawData = pd.read_csv('AuthorsDataset.csv',encoding = "ISO-8859-1")

stemmer = nltk.stem.porter.PorterStemmer()
remove_punctuation_map = dict((ord(char), None) for char in string.punctuation)

def stem_tokens(tokens):
    return [stemmer.stem(item) for item in tokens]

'''remove punctuation, lowercase, stem'''
def normalize(text):
    return stem_tokens(nltk.word_tokenize(text.lower().translate(remove_punctuation_map)))

vectorizer = TfidfVectorizer(tokenizer=normalize, stop_words='english')


print(rawData[[0,0]])

