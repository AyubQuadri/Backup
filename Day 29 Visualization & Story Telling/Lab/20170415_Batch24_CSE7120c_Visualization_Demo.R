#####DATA VISUALIZATION###
rm(list=ls(all=TRUE))
setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 29 Visualization & Story Telling/Lab")

data<-read.csv("UniversalBank.csv",header=T)  
attach(data)
names(data) 

# HISTOGRAMS: for numerical attributes
par(mfrow=c(1,3))

hist(CCAvg, col="red", xlab="CC Avg",main="Histogram for CCAvg") 
hist(Age, col="green", xlab="Age",main="Histogram for Age")
hist(Experience, col="blue", xlab="Experience",main="Histogram for Experience")


# BOX PLOTS: best for numerical attributes
par(mfrow=c(1,2))
boxplot(Income, main="Income distribution")
abline(h = mean(Income), lty=2)
boxplot(Income~Education, main="Education Vs. Income", 
        xlab="Education", ylab="Income")
abline(h = mean(Income), lty=2)

# BAR PLOTS: best for categorical attributes
table(data$Personal.Loan) # or summary(as.factor(data$Personal.Loan)) 

par(mfrow=c(1,1))
barplot(table(data$Personal.Loan), 
        main="Number of customer in each class")

#Sidenote: compare Hist vs. Bar Plot for numerical attribute
par(mfrow=c(1,2))
barplot(Income) #not suitable - a peak for every point
hist(Income) #suitable as the income is binned

# SCATTER PLOTS: to see correlation between 2 numeric attributes
par(mfrow=c(1,1))
plot(Age,Income)
plot(Experience,Income)

# VISUALIZATION USING "ggplot" (evolutionary steps)
library(ggplot2)

# BAR PLOT (Personal Loan)

# Start with a basic graph  in ggplot
ggplot(data,aes(x=as.factor(Personal.Loan)))+geom_bar()+
  xlab("Loan")+ylab("No. of Customers") 

# Add color & change back ground
ggplot(data,
       aes(x=as.factor(Personal.Loan),fill=as.factor(Personal.Loan)))+geom_bar()+
  xlab("Loan")+ylab("No. of Customers")

ggplot(data,
       aes(x=as.factor(Personal.Loan),fill=as.factor(Personal.Loan)))+geom_bar()+
  xlab("Loan")+ylab("No. of Customers")+theme_bw()

# Add titles & adjust text size of labels & axes
ggplot(data,
       aes(x=as.factor(Personal.Loan),fill=as.factor(Personal.Loan)))+geom_bar()+
  xlab("Loan")+ylab("No. of Customers")+theme_bw()+
  theme(axis.text.x=element_text(hjust=4,angle=45,size=14),text=element_text(size=14))

# Now removing the legend position
ggplot(data,
       aes(x=as.factor(Personal.Loan),fill=as.factor(Personal.Loan)))+geom_bar()+
  xlab("Loan")+ylab("No. of Customers")+theme_bw()+
  theme(axis.text.x=element_text(hjust=1,angle=45,size=14),text=element_text(size=14),
        legend.position="none")

# Add counts on top of each bin
ggplot(data,
       aes(x=as.factor(Personal.Loan),fill=as.factor(Personal.Loan)))+geom_bar()+
  geom_text(stat='count',aes(label=..count..),vjust=0)+
  xlab("Loan")+ylab("No. of Customers")+theme_bw()+
  theme(axis.text.x=element_text(hjust=1,angle=45,size=18),text=element_text(size=20),legend.position="none")

# BAR PLOT (Education)  
ggplot(data=data,
       aes(x=as.factor(Education)))+
       geom_bar(width=0.5)+theme_bw()+
       theme(axis.text.x = element_text(angle=0,hjust=1),
       text = element_text(size=14),
       legend.position="none")+xlab("Education")+
       ylab("No. of Customers")

## SCATTER PLOT (2 dimensions)
ggplot(data=data, aes(x=Age, y=Income))+
      geom_point()

ggplot(data=data,aes(x=Age, y=Income))+
      geom_point()+theme_bw()+xlab("Age")+ylab("Income")

# SCATTER PLOT (3 dimensions)
ggplot(data=data,
       aes(x=Age, y=Income, color=as.factor(Personal.Loan)))+
  geom_point()+theme_bw()+xlab("Age")+ylab("Income")

# SCATTER PLOT (3 dimensions with jitter)
ggplot(data=data,
       aes(x=Age, y=Income, color=as.factor(Personal.Loan)))+
  geom_jitter()+theme_bw()+xlab("Age")+ylab("Income")

# SCATTER PLOT (4 dimensions)
ggplot(data=data,
       aes(x=as.factor(Education), y=Income, color=as.factor(Personal.Loan),
           size=CCAvg))+
  geom_point()+
  scale_size_area(max_size=9)+
  xlab("Educational qualifications") +
  ylab("Income") +
  theme_bw()+
  theme(axis.text.x=element_text(size=18),
        axis.title.x = element_text(size =18,
                                    colour = 'black'))+
  theme(axis.text.y=element_text(size=18),
        axis.title.y = element_text(size = 18,
                                    colour = 'black',angle = 90))

# SCATTER PLOT (4 dimensions with Jitter)
ggplot(data=data,
       aes(x=as.factor(Education), y=Income, color=as.factor(Personal.Loan), size=CCAvg))+
  geom_jitter()+
  xlab("Educational Qualifications") +
  ylab("Income") +
  theme_bw()+
  theme(axis.text.x=element_text(size=18),
        axis.title.x = element_text(size =18,
                                    colour = 'black'))+
  theme(axis.text.y=element_text(size=18),
        axis.title.y = element_text(size = 18,
                                    colour = 'black',angle = 90))


#hist(CCAvg, col="red", xlab="CC Avg",main="Histogram for CCAvg")
p1 <- ggplot(data = data)
p1 + geom_histogram(aes(x = CCAvg), binwidth = 2)

# "mpg" buit-in dataset (to demonstrate faceting)
rm(list=ls(all=TRUE))
p1 <- ggplot(data = mpg)
p2 <- aes(x = displ, y = cty)
p1 + p2 + geom_point()
p1 + p2 + geom_point(aes(color = factor(cyl)))
box <- p1 + aes(x = class, y = cty) + geom_boxplot()
box
#box + aes(color = factor(cyl))
box1 <- p1 + aes(x = class, y = cty)+
      geom_boxplot(outlier.colour = "green", outlier.size = 3)
box1        

box1 + aes(color = factor(cyl))
#This looks Ugly! So we opt for faceting
# Faceting <- To give each factor it's own subplot
box1 + facet_wrap(~cyl) + aes(color = class)

#REFERENCE: http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/

# USING BUILT IN DATASET

###Group bar charts
c <- ggplot(mtcars, aes(factor(cyl)))
c + geom_bar()
c+geom_bar(width=0.5)
c + geom_bar() +coord_flip()
#c + scale_fill_brewer()
c+geom_bar(fill="green", colour="darkgreen", width=0.5)

##Stacked bar plots
qplot(factor(cyl), data=mtcars, geom="bar")
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(vs))
qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))

###Boxplots
p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_boxplot()
p + geom_boxplot() + geom_jitter()
p + geom_boxplot(outlier.colour = "green", outlier.size = 3) + geom_jitter()
p + geom_boxplot(aes(fill=cyl))
p + geom_boxplot(aes(color=factor(cyl), stats="identity"))

###Scatterplot
plot(mtcars$mpg,mtcars$disp)
##Task- label the axes, give the title  and plot with lines

#connect points with line, #add regression line
p1 <- ggplot(mtcars, aes(x = hp, y = mpg))
p1 + geom_point(color="blue")
p1 + geom_point(color="blue") + geom_line()                           
p1 + geom_point(color="red") + geom_smooth(method = "lm", se = FALSE)  

##Visulisations using Deducer
#install.packages("JGR")
library(JGR)
JGR()

