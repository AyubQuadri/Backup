# -*- coding: utf-8 -*-
"""
Created on Wed Mar 23 13:17:54 2016

@author: sanjay
"""

import os
os.getcwd
import pandas as pd
import numpy as np
os.chdir('C:\\Users\\quadris\\Desktop\\My Learning\\INSOFE CPEE\\Day 22 Sentiment Analysis & summarization\\Lab Activity\\Data\Data')
Grade1=pd.read_csv("Grade1.csv",header=0) #reading CSV file
Grade2=pd.read_csv("Grade2.csv",header=0)
print(Grade1.shape) # Dimensions of data frame
print(Grade1.size)  
print(Grade1.dtypes) #structure of dataframe
print(Grade1.head()) # first five observations of data frame
print(Grade1.tail()) # last five observations of data frame
print(Grade1['Student id'].iloc[0]) # first element in student.id column
print(Grade1['Student id'][1:5]) #  element with index 1 to 4 in student.id column
print(Grade1['Student id'].iloc[0:8]) # first 9 elements
print(Grade1['English1']) # entire column
print(Grade1.iloc[0:20,0:5]) #whole data frame through indexing
print(Grade1.iloc[:,0:3]) # choosing columns to print
print(Grade1[['Student id','English1']])
d=['English1','Science1']
print(Grade1[d])
print(Grade1[Grade1.English1>45]) # english marks greater than 45 records
Grade1['Student id']= Grade1['Student id'].astype('category') # changing the type of variables
result = pd.merge(Grade1,Grade2,how='outer',on='Student id') # merging of two data frames based on key values
Grade1['Class']="NULL"
for i in range (0,20):
    if (Grade1['OverallPct1'][i]<40):
        Grade1['Class'][i]="C"
    else:
        if (Grade1['OverallPct1'][i]<60):
            Grade1['Class'][i]="B"
        else:
            Grade1['Class'][i]="A"
def summaryx(x):

     a=x.mean()
     b=x.median()
     c=x.std()
     d=x.quantile(0.25)
     e=x.quantile(0.75)
     summary_x=pd.DataFrame([a,b,c,d,e],index=['mean','median','std','1-quantile','3-quantile'])
     return summary_x;
Cust=pd.read_csv("CustTransDat.csv",header=0)
Cust_dcast=Cust.pivot_table(index=['Quarter','Month'],columns='Year',values='Cost',aggfunc=sum)# Dcasting according to quarter+month~year on cost
Grade1_melt=pd.melt(Grade1,id_vars=['Student id']) #melting data
# Working on Airfares data
data = pd.read_csv("Airfares.csv",converters={'FARE': lambda  x:float(x.replace('$',""))})# removing dollar sign from fare column
data['S_STATE']=data.S_CITY.str[0:20] # separating state and city column
data['S_CITY']=data.S_CITY.str[20:22]#separating state and city column
data['E_STATE']=data.E_CITY.str[0:20]#separating state and city column
data['E_CITY']=data.E_CITY.str[20:22]#separating state and city column
cols= [0,18,1,2,19,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]
data=data[cols]
# removing unwanted symbols from income columns
import re
data['S_INCOME']=data['S_INCOME'].apply(lambda x:re.sub(r'[$,]',r'',x))
data['E_INCOME']=data['E_INCOME'].apply(lambda x:re.sub(r'[$,]',r'',x)).astype(np.float64)
# finding mean fare on different conditions
print(data.groupby('VACATION').agg({'FARE': np.mean}))
print(data.groupby('SLOT').agg({'FARE': np.mean}))
print(data.groupby('GATE').agg({'FARE': np.mean}))

# working on subscribers data
data1 = pd.read_csv("subscribersData.csv")
data1['SUBS_MSISDN']=data1['SUBS_MSISDN'].astype('category')
data1['SUBS_MSISDN']=data1['SUBS_MSISDN'].cat.rename_categories(['a','b','c','d','e'])
# considering not null values
data1= data1[pd.notnull(data1['CUST_REV'])]
# extracting week number and month from date
import datetime
data1['week']=data1['EVENT_DT'].apply(lambda x:datetime.datetime.strptime(x, "%Y-%m-%d").strftime('%U'))
data1['month']=data1['EVENT_DT'].apply(lambda x:datetime.datetime.strptime(x, "%Y-%m-%d").strftime('%m'))
# aggregating on id,week and month
mean= data1.groupby(['SUBS_MSISDN','week','month']).agg({'CUST_REV': np.mean})

# Working with missing data
dataMerge=pd.read_csv("dataMerged.csv",header=0)
no_na=(dataMerge.isnull().sum()).sum()# calculating total na values
dataMerge1=dataMerge.dropna(axis=0)#dropping NA values
dataMerge2=dataMerge.fillna(dataMerge.mean())# Similar to central imputation 

# Standardizing the data
from sklearn import preprocessing
std_scale = preprocessing.StandardScaler().fit(dataMerge2[['inc']])
df_std = std_scale.transform(dataMerge2[['inc']])

minmax_scale = preprocessing.MinMaxScaler().fit(dataMerge2[['inc']])
df_minmax = minmax_scale.transform(dataMerge2[['inc']])

#descretizing the data
data_Merge_INC=dataMerge2['inc']
factor=pd.cut(data_Merge_INC,4,labels=[1,2,3,4])

#dummies
factor_dummies=pd.get_dummies(factor)

#Visualizations

import matplotlib.pyplot as plt
data_imputed=pd.read_csv("data_imputed.csv",header=0)
#histogram
plt.hist(data_imputed['inc'],color='r',label="Income")
plt.title("Income Histogram")
plt.show()
plt.figure(1)
plt.subplot(211)
plt.hist(data_imputed['age'],color='r',label="age")
plt.subplot(212)
plt.hist(data_imputed['exp'],color='b',label="exp")
plt.show()

# boxplot
plt.figure(2)
plt.subplot(211)
plt.boxplot(data_imputed['age'])
plt.title("age")
plt.subplot(212)
plt.boxplot(data_imputed['exp'])
plt.title("exp")
plt.show()

#Sampling of data
import random
from sklearn.cross_validation import train_test_split
train,test = train_test_split(dataMerge2, test_size = 0.333333)# splitting dataset into train and test
X=dataMerge2.ix[random.sample(dataMerge2.index,1500)] # randomly selecting 1500 observations from dataset

# Writing data to CSV
Cust_dcast.to_csv("Cust_dcast.csv")