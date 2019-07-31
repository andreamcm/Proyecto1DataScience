install.packages("reshape")
library(reshape)

setwd("C:/Users/user/Documents/2019/UVG/Segundo Semestre/DataScience/Proyectos/Proyecto1/Proyecto1DataScience/DatosCrudos")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)
data <- merge_recurse(myfiles)
