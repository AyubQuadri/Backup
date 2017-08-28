rm(list=ls(all=TRUE))
library(data.table)
library(ggplot2)

setwd("D:/AMITS_LABS/BATCH 24/Day31_VISUALIZATIONS")
df <- read.csv('EconomistData.csv')
df <-df[,-1]
head(df)
attach(df) 

# Create a scatter plot with CPI on the x axis and HDI on the y axis.
p1 <- ggplot(df, aes(x=CPI,y=HDI))+
      geom_point() 
p1

#Map the color of the points to Region.
p2 <- p1+ aes(color=Region)+
      geom_point(shape=1,size=2) 
p2

#Add trendline
p3 <- p2 + geom_smooth(aes(group=1), method = 'lm', 
      formula = y~log(x), se=FALSE, color='red')
p3

# Add country labels (select only a few)
names <- c("Russia","Venezuela", "Iraq", "Myanmar", "Sudan",
                  "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                  "India", "Italy", "China", "South Africa", "Spane",
                  "Botswana", "Cape Verde", "Bhutan", "Australia", "France",
                  "United States", "Germany", "Britain", "Barbados", "Norway",
                  "Japan","New Zealand", "Singapore")

p4 <- p3 + geom_text(aes(label=Country), color='gray20', 
      data = subset(df, Country %in% names),check_overlap = TRUE) 
p4

#Modify the x, y, and color scales in the plot 
p5 <- p4 + 
      scale_x_continuous(name = "Corruption Perceptions Index, 
                         2011",limits = c(1, 10),
                         breaks=1:10)+
      scale_y_continuous(name = "Human Development Index,2011",
                         limits = c(0.2, 1.0))
p5

# Add chart title
p6 <- p5 + ggtitle("Corruption and Human development")
p6

#Change theme
#install.packages("ggthemes")
library(ggthemes)
p6 + theme_economist_white()

p7 <-p6+aes(size=HDI.Rank)
p7 


