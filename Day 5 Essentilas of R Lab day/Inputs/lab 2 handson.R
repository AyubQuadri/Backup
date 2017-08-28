# clearing evenironment #

rm(list=c("m","x","y","z"))
rm(list = ls())

# 3 ways to set the path
#1 ctrl+Shift+H browse and set the path
setwd()
getwd()

#2 setworking directory by specifiying the path
setwd()

Grade <-read.csv("Grade1.csv", header = T, sep=",")
read <- read.table("Grade1.csv", header = TRUE, sep = ",")
read

read <- read.table("greek.txt", header = TRUE, sep = "\t")
read


#reading from excel file#

install.packages("XLConnect")
require(XLConnect)
wb <- loadWorkbook("Book3.xlsx")
sheet <- readWorksheet(wb,"Sheet1",header=T)
sheet

#output file#
setwd()
getwd()

write.csv(Grade, "data.csv", row.names=F) #check once
library(xlsx)
write.xlsx(sheet, "write.xlsx", row.names= F)

# if condition #

#Eg 1
x <- 20
if(is.numeric(x))
{
  print("Numeric");
}

x <- c("what","is","truth")

#Eg 2
if("truth" %in% x) {
  print("Truth is found")
} else {
  print("Truth is not found")
}

#For Loop

#Eg 1
v <- LETTERS[1:4]
for ( i in v) {
  print(i)
}

#while loop

v <- c("Hello","while loop")
cnt <- 0

while (cnt < 4) {
  print(v)
  cnt = cnt + 1
}

v <- c("Hello","while loop")
cnt <- 0

while (cnt < 4) {
  print(v[2])
  cnt = cnt + 1
}

