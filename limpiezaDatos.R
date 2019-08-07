install.packages("reshape")
library(reshape)

install.packages("dplyr")
library(dplyr)

setwd("C:/Users/user/Documents/2019/UVG/Segundo Semestre/DataScience/Proyectos/Proyecto1/Proyecto1DataScience/DatosCrudos")
wd <- "C:/Users/user/Documents/2019/UVG/Segundo Semestre/DataScience/Proyectos/Proyecto1/Proyecto1DataScience/DatosCrudos"
temp = list.files(pattern="*.csv")

altaverapaz <- read.csv(temp[1], header = TRUE)
bajaverapaz <- read.csv(temp[2], header = TRUE)
chimaltenango <- read.csv(temp[3], header = TRUE)
chiquimula <- read.csv(temp[4], header = TRUE)
ciudadcapital <- read.csv(temp[5], header = TRUE)
elprogreso <- read.csv(temp[6], header = TRUE)
escuintla <- read.csv(temp[7], header = TRUE)
guatemala <- read.csv(temp[8], header = TRUE)
huehuetenango <- read.csv(temp[9], header = TRUE)
izabal <- read.csv(temp[10], header = TRUE)
jalapa <- read.csv(temp[11], header = TRUE)
jutiapa <- read.csv(temp[12], header = TRUE)
peten <- read.csv(temp[13], header = TRUE)
quetzaltenango <- read.csv(temp[14], header = TRUE)
quiche <- read.csv(temp[15], header = TRUE)
retalhuleu <- read.csv(temp[16], header = TRUE)
sacatepequez <- read.csv(temp[17], header = TRUE)
sanmarcos <- read.csv(temp[18], header = TRUE)
santarosa <- read.csv(temp[19], header = TRUE)
solola <- read.csv(temp[20], header = TRUE)
suchitepequez <- read.csv(temp[21], header = TRUE)
totonicapan <- read.csv(temp[22], header = TRUE)
zacapa <- read.csv(temp[23], header = TRUE)

# Revisar que las columnas sean todas iguales... que molesta R
names(bajaverapaz) <- names(altaverapaz)
names(chimaltenango) <- names(altaverapaz)
names(chiquimula) <- names(altaverapaz)
names(ciudadcapital) <- names(altaverapaz)
names(elprogreso) <- names(altaverapaz)
names(escuintla) <- names(altaverapaz)
names(guatemala) <- names(altaverapaz)
names(huehuetenango) <- names(altaverapaz)
names(izabal) <- names(altaverapaz)
names(jutiapa) <- names(altaverapaz)
names(jalapa) <- names(altaverapaz)
names(peten) <- names(altaverapaz)
names(quetzaltenango) <- names(altaverapaz)
names(quiche) <- names(altaverapaz)
names(retalhuleu) <- names(altaverapaz)
names(sacatepequez) <- names(altaverapaz)
names(suchitepequez) <- names(altaverapaz)
names(sanmarcos) <- names(altaverapaz)
names(santarosa) <- names(altaverapaz)
names(solola) <- names(altaverapaz)
names(totonicapan) <- names(altaverapaz)
names(zacapa) <- names(altaverapaz)
identical(names(altaverapaz), names(bajaverapaz))

# Union de todos los departamentos
todosDepartamentos <- rbind(altaverapaz, bajaverapaz, chimaltenango, chiquimula, ciudadcapital, elprogreso, escuintla, guatemala, huehuetenango, izabal, jalapa, jutiapa, peten, quetzaltenango, quiche, retalhuleu, sacatepequez, sanmarcos, santarosa, solola, suchitepequez, totonicapan, zacapa)

# Eliminación de duplicados
options(max.print= 1000000000)
unique(todosDepartamentos)

duplicated(todosDepartamentos)
no_dup <-todosDepartamentos %>% distinct()
names(no_dup) <- as.matrix(no_dup[1, ])
no_dup <- no_dup[-1, ]
no_dup[] <- lapply(no_dup, function(x) type.convert(as.character(x)))
                   
# Estandar para telefonos

no_dup$TELEFONO <- as.character(no_dup$TELEFONO)
no_dup$TELEFONO[nchar(as.character(no_dup$TELEFONO))<8 & !is.na(no_dup$TELEFONO)] <- NA
no_dup$TELEFONO[nchar(as.character(no_dup$TELEFONO))>8 & !is.na(no_dup$TELEFONO)] <- substr(no_dup$TELEFONO, 1, 8)

                   
