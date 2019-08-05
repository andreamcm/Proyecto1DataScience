install.packages("reshape")
library(reshape)

install.packages("dplyr")
library(dplyr)

setwd("C:\\Users\\User\\Desktop\\Data Science\\Proyecto1DataScience\\DatosCrudos")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)
data <- merge_recurse(myfiles)

options(max.print= 1000000000)
unique(data)

duplicated(data)
x <-data %>% distinct()
