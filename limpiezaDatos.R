install.packages("reshape")
library(reshape)

install.packages("dplyr")
library(dplyr)

install.packages("stringr")
library(stringr)

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

# EliminaciÃ³n de duplicados
options(max.print= 1000000000)
unique(todosDepartamentos)

duplicated(todosDepartamentos)
no_dup <-todosDepartamentos %>% distinct()
names(no_dup) <- as.matrix(no_dup[1, ])
no_dup <- no_dup[-1, ]
no_dup[] <- lapply(no_dup, function(x) type.convert(as.character(x)))
no_dup <- no_dup[!(is.na(no_dup$ESTABLECIMIENTO) | no_dup$ESTABLECIMIENTO==""), ]
                   
# Estandar para telefonos

no_dup$TELEFONO <- as.character(no_dup$TELEFONO)
no_dup$TELEFONO[nchar(as.character(no_dup$TELEFONO))<8 & !is.na(no_dup$TELEFONO)] <- NA
no_dup$TELEFONO[nchar(as.character(no_dup$TELEFONO))>8 & !is.na(no_dup$TELEFONO)] <- substr(no_dup$TELEFONO, 1, 8)

#Status -> abierta = 1, temporalmente cerrado = 0    
no_dup$STATUS <- gsub("ABIERTA", 1, no_dup$STATUS)
no_dup$STATUS <- gsub("CERRADA TEMPORALMENTE", 0, no_dup$STATUS)

#Estandar para vacío
no_dup$CODIGO <- as.character(no_dup$CODIGO)
no_dup$DISTRITO <- as.character(no_dup$DISTRITO)
no_dup$DEPARTAMENTO <- as.character(no_dup$DEPARTAMENTO)
no_dup$MUNICIPIO <- as.character(no_dup$MUNICIPIO)
no_dup$ESTABLECIMIENTO <- as.character(no_dup$ESTABLECIMIENTO)
no_dup$DIRECCION <- as.character(no_dup$DIRECCION)
no_dup$TELEFONO <- as.character(no_dup$TELEFONO)
no_dup$SUPERVISOR <- as.character(no_dup$SUPERVISOR)
no_dup$DIRECTOR <- as.character(no_dup$DIRECTOR)
no_dup$NIVEL <- as.character(no_dup$NIVEL)
no_dup$SECTOR <- as.character(no_dup$SECTOR)
no_dup$AREA <- as.character(no_dup$AREA)
no_dup$STATUS <- as.character(no_dup$STATUS)
no_dup$MODALIDAD <- as.character(no_dup$MODALIDAD)
no_dup$JORNADA <- as.character(no_dup$JORNADA)
no_dup$PLAN <- as.character(no_dup$PLAN)
no_dup$DEPARTAMENTAL <- as.character(no_dup$DEPARTAMENTAL)

#Cambio en CODIGO
no_dup$CODIGO[no_dup$CODIGO == ""] <- NA
no_dup$CODIGO[no_dup$CODIGO == "-"] <- NA
no_dup$CODIGO[no_dup$CODIGO == "--"] <- NA
no_dup$CODIGO[no_dup$CODIGO == "0"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="---"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="-----"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="----"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="----------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="-----------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="-----------------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="---------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="--------------------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="--------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="-------------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="-------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="-------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="--------------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="---------------------"] <- NA
no_dup$CODIGO[no_dup$CODIGO=="-----------------------------"] <- NA

#Cambio en DISTRITO
no_dup$DISTRITO[no_dup$DISTRITO == ""] <- NA
no_dup$DISTRITO[no_dup$DISTRITO == "-"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO == "--"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO == "0"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="---"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="-----"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="----"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="----------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="-----------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="-----------------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="---------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="--------------------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="--------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="-------------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="-------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="-------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="--------------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="---------------------"] <- NA
no_dup$DISTRITO[no_dup$DISTRITO=="-----------------------------"] <- NA

#Cambio en DEPARTAMENTO
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO == ""] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO == "-"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO == "--"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO == "0"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="---"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="-----"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="----"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="----------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="-----------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="-----------------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="---------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="--------------------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="--------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="-------------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="-------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="-------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="--------------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="---------------------"] <- NA
no_dup$DEPARTAMENTO[no_dup$DEPARTAMENTO=="-----------------------------"] <- NA

#Cambio en MUNICIPIO
no_dup$MUNICIPIO[no_dup$MUNICIPIO == ""] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO == "-"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO == "--"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO == "0"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="---"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="-----"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="----"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="----------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="-----------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="-----------------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="---------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="--------------------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="--------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="-------------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="-------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="-------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="--------------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="---------------------"] <- NA
no_dup$MUNICIPIO[no_dup$MUNICIPIO=="-----------------------------"] <- NA

#Cambio en ESTABLECIMIENTO
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO == ""] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO == "-"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO == "--"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO == "0"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="---"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="-----"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="----"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="----------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="-----------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="-----------------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="---------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="--------------------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="--------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="-------------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="-------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="-------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="--------------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="---------------------"] <- NA
no_dup$ESTABLECIMIENTO[no_dup$ESTABLECIMIENTO=="-----------------------------"] <- NA

#Cambio en DIRECCION
no_dup$DIRECCION[no_dup$DIRECCION == ""] <- NA
no_dup$DIRECCION[no_dup$DIRECCION == "-"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION == "--"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION == "0"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="---"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="-----"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="----"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="----------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="-----------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="-----------------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="---------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="--------------------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="--------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="-------------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="-------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="-------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="--------------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="---------------------"] <- NA
no_dup$DIRECCION[no_dup$DIRECCION=="-----------------------------"] <- NA

#Cambio en TELEFONO
no_dup$TELEFONO[no_dup$TELEFONO == ""] <- NA
no_dup$TELEFONO[no_dup$TELEFONO == "-"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO == "--"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO == "0"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="---"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="-----"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="----"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="----------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="-----------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="-----------------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="---------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="--------------------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="--------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="-------------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="-------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="-------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="--------------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="---------------------"] <- NA
no_dup$TELEFONO[no_dup$TELEFONO=="-----------------------------"] <- NA

#Cambio en SUPERVISOR
no_dup$SUPERVISOR[no_dup$SUPERVISOR == ""] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR == "-"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR == "--"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR == "0"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="---"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="-----"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="----"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="----------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="-----------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="-----------------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="---------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="--------------------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="--------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="-------------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="-------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="-------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="--------------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="---------------------"] <- NA
no_dup$SUPERVISOR[no_dup$SUPERVISOR=="-----------------------------"] <- NA

#Cambio en DIRECTOR
no_dup$DIRECTOR[no_dup$DIRECTOR == ""] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR == "-"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR == "--"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR == "0"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="---"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="-----"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="----"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="----------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="-----------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="-----------------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="---------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="--------------------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="--------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="-------------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="-------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="-------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="--------------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="---------------------"] <- NA
no_dup$DIRECTOR[no_dup$DIRECTOR=="-----------------------------"] <- NA

#Cambio en NIVEL
no_dup$NIVEL[no_dup$NIVEL == ""] <- NA
no_dup$NIVEL[no_dup$NIVEL == "-"] <- NA
no_dup$NIVEL[no_dup$NIVEL == "--"] <- NA
no_dup$NIVEL[no_dup$NIVEL == "0"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="---"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="-----"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="----"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="----------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="-----------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="-----------------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="---------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="--------------------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="--------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="-------------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="-------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="-------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="--------------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="---------------------"] <- NA
no_dup$NIVEL[no_dup$NIVEL=="-----------------------------"] <- NA

#Cambio en SECTOR
no_dup$SECTOR[no_dup$SECTOR == ""] <- NA
no_dup$SECTOR[no_dup$SECTOR == "-"] <- NA
no_dup$SECTOR[no_dup$SECTOR == "--"] <- NA
no_dup$SECTOR[no_dup$SECTOR == "0"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="---"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="-----"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="----"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="----------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="-----------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="-----------------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="---------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="--------------------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="--------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="-------------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="-------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="-------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="--------------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="---------------------"] <- NA
no_dup$SECTOR[no_dup$SECTOR=="-----------------------------"] <- NA

#Cambio en AREA
no_dup$AREA[no_dup$AREA == ""] <- NA
no_dup$AREA[no_dup$AREA == "-"] <- NA
no_dup$AREA[no_dup$AREA == "--"] <- NA
no_dup$AREA[no_dup$AREA == "0"] <- NA
no_dup$AREA[no_dup$AREA=="---"] <- NA
no_dup$AREA[no_dup$AREA=="-----"] <- NA
no_dup$AREA[no_dup$AREA=="------"] <- NA
no_dup$AREA[no_dup$AREA=="----"] <- NA
no_dup$AREA[no_dup$AREA=="----------"] <- NA
no_dup$AREA[no_dup$AREA=="-----------"] <- NA
no_dup$AREA[no_dup$AREA=="-----------------"] <- NA
no_dup$AREA[no_dup$AREA=="---------"] <- NA
no_dup$AREA[no_dup$AREA=="--------------------"] <- NA
no_dup$AREA[no_dup$AREA=="--------"] <- NA
no_dup$AREA[no_dup$AREA=="-------------"] <- NA
no_dup$AREA[no_dup$AREA=="-------"] <- NA
no_dup$AREA[no_dup$AREA=="-------"] <- NA
no_dup$AREA[no_dup$AREA=="--------------"] <- NA
no_dup$AREA[no_dup$AREA=="---------------------"] <- NA
no_dup$AREA[no_dup$AREA=="-----------------------------"] <- NA

#Cambio en STATUS
no_dup$STATUS[no_dup$STATUS == ""] <- NA
no_dup$STATUS[no_dup$STATUS == "-"] <- NA
no_dup$STATUS[no_dup$STATUS == "--"] <- NA
no_dup$STATUS[no_dup$STATUS == "0"] <- NA
no_dup$STATUS[no_dup$STATUS=="---"] <- NA
no_dup$STATUS[no_dup$STATUS=="-----"] <- NA
no_dup$STATUS[no_dup$STATUS=="------"] <- NA
no_dup$STATUS[no_dup$STATUS=="----"] <- NA
no_dup$STATUS[no_dup$STATUS=="----------"] <- NA
no_dup$STATUS[no_dup$STATUS=="-----------"] <- NA
no_dup$STATUS[no_dup$STATUS=="-----------------"] <- NA
no_dup$STATUS[no_dup$STATUS=="---------"] <- NA
no_dup$STATUS[no_dup$STATUS=="--------------------"] <- NA
no_dup$STATUS[no_dup$STATUS=="--------"] <- NA
no_dup$STATUS[no_dup$STATUS=="-------------"] <- NA
no_dup$STATUS[no_dup$STATUS=="-------"] <- NA
no_dup$STATUS[no_dup$STATUS=="-------"] <- NA
no_dup$STATUS[no_dup$STATUS=="--------------"] <- NA
no_dup$STATUS[no_dup$STATUS=="---------------------"] <- NA
no_dup$STATUS[no_dup$STATUS=="-----------------------------"] <- NA

#Cambio en MODALIDAD
no_dup$MODALIDAD[no_dup$MODALIDAD == ""] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD == "-"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD == "--"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD == "0"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="---"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="-----"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="----"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="----------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="-----------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="-----------------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="---------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="--------------------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="--------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="-------------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="-------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="-------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="--------------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="---------------------"] <- NA
no_dup$MODALIDAD[no_dup$MODALIDAD=="-----------------------------"] <- NA

#Cambio en JORNADA
no_dup$JORNADA[no_dup$JORNADA == ""] <- NA
no_dup$JORNADA[no_dup$JORNADA == "-"] <- NA
no_dup$JORNADA[no_dup$JORNADA == "--"] <- NA
no_dup$JORNADA[no_dup$JORNADA == "0"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="---"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="-----"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="----"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="----------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="-----------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="-----------------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="---------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="--------------------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="--------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="-------------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="-------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="-------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="--------------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="---------------------"] <- NA
no_dup$JORNADA[no_dup$JORNADA=="-----------------------------"] <- NA

#Cambio en PLAN
no_dup$PLAN[no_dup$PLAN == ""] <- NA
no_dup$PLAN[no_dup$PLAN == "-"] <- NA
no_dup$PLAN[no_dup$PLAN == "--"] <- NA
no_dup$PLAN[no_dup$PLAN == "0"] <- NA
no_dup$PLAN[no_dup$PLAN=="---"] <- NA
no_dup$PLAN[no_dup$PLAN=="-----"] <- NA
no_dup$PLAN[no_dup$PLAN=="------"] <- NA
no_dup$PLAN[no_dup$PLAN=="----"] <- NA
no_dup$PLAN[no_dup$PLAN=="----------"] <- NA
no_dup$PLAN[no_dup$PLAN=="-----------"] <- NA
no_dup$PLAN[no_dup$PLAN=="-----------------"] <- NA
no_dup$PLAN[no_dup$PLAN=="---------"] <- NA
no_dup$PLAN[no_dup$PLAN=="--------------------"] <- NA
no_dup$PLAN[no_dup$PLAN=="--------"] <- NA
no_dup$PLAN[no_dup$PLAN=="-------------"] <- NA
no_dup$PLAN[no_dup$PLAN=="-------"] <- NA
no_dup$PLAN[no_dup$PLAN=="-------"] <- NA
no_dup$PLAN[no_dup$PLAN=="--------------"] <- NA
no_dup$PLAN[no_dup$PLAN=="---------------------"] <- NA
no_dup$PLAN[no_dup$PLAN=="-----------------------------"] <- NA

#Cambio en DEPARTAMENTAL
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL == ""] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL == "-"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL == "--"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL == "0"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="---"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="-----"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="----"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="----------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="-----------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="-----------------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="---------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="--------------------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="--------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="-------------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="-------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="-------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="--------------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="---------------------"] <- NA
no_dup$DEPARTAMENTAL[no_dup$DEPARTAMENTAL=="-----------------------------"] <- NA

# Con este código se puede verificar si existen más datos raros dentro de las variables
summary(no_dup$STATUS)
grep("DIARIO", no_dup$PLAN, value = TRUE)

# Nueva columna para numero de departamento
temp
no_dup$CODIGO_DEPARTAMENTO <- 0
# No sé si se tiene que correr antes, pero mejor hacerlo para que funcione bien
no_dup$CODIGO_DEPARTAMENTO <- gsub("ALTA VERAPAZ", 1, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("BAJA VERAPAZ", 2, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("CHIMALTENANGO", 3, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("CHIQUIMULA", 4, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("CIUDAD CAPITAL", 5, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("EL PROGRESO", 6, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("ESCUINTLA", 7, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("GUATEMALA", 8, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("HUEHUETENANGO", 9, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("IZABAL", 10, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("JALAPA", 11, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("JUTIAPA", 12, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("PETEN", 13, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("QUETZALTENANGO", 14, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("QUICHE", 15, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("RETALULEU", 16, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("SACATEPEQUEZ", 17, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("SAN MARCOS", 18, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("SANTA ROSA", 19, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("SOLOLA", 20, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("SUCHITEPEQUEZ", 21, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("TOTONICAPAN", 22, no_dup$DEPARTAMENTO)
no_dup$CODIGO_DEPARTAMENTO <- gsub("ZACAPA", 23, no_dup$DEPARTAMENTO)

# Esto es lo que cambia el codigo
# Alta Verapaz = 1
no_dup$CODIGO_DEPARTAMENTO <- gsub("ALTA VERAPAZ", 1, no_dup$CODIGO_DEPARTAMENTO)
# Baja Verapaz = 2
no_dup$CODIGO_DEPARTAMENTO <- gsub("BAJA VERAPAZ", 2, no_dup$CODIGO_DEPARTAMENTO)
# Chimaltenango = 3
no_dup$CODIGO_DEPARTAMENTO <- gsub("CHIMALTENANGO", 3, no_dup$CODIGO_DEPARTAMENTO)
# Chiquimula = 4
no_dup$CODIGO_DEPARTAMENTO <- gsub("CHIQUIMULA", 4, no_dup$CODIGO_DEPARTAMENTO)
# Ciudad Capital = 5
no_dup$CODIGO_DEPARTAMENTO <- gsub("CIUDAD CAPITAL", 5, no_dup$CODIGO_DEPARTAMENTO)
# El Progreso = 6
no_dup$CODIGO_DEPARTAMENTO <- gsub("EL PROGRESO", 6, no_dup$CODIGO_DEPARTAMENTO)
# Escuintla = 7
no_dup$CODIGO_DEPARTAMENTO <- gsub("ESCUINTLA", 7, no_dup$CODIGO_DEPARTAMENTO)
# Guatemala = 8
no_dup$CODIGO_DEPARTAMENTO <- gsub("GUATEMALA", 8, no_dup$CODIGO_DEPARTAMENTO)
# Huehuetenango = 9
no_dup$CODIGO_DEPARTAMENTO <- gsub("HUEHUETENANGO", 9, no_dup$CODIGO_DEPARTAMENTO)
# Izabal = 10
no_dup$CODIGO_DEPARTAMENTO <- gsub("IZABAL", 10, no_dup$CODIGO_DEPARTAMENTO)
# Jalapa = 11
no_dup$CODIGO_DEPARTAMENTO <- gsub("JALAPA", 11, no_dup$CODIGO_DEPARTAMENTO)
# Jutiapa = 12
no_dup$CODIGO_DEPARTAMENTO <- gsub("JUTIAPA", 12, no_dup$CODIGO_DEPARTAMENTO)
# Peten = 13
no_dup$CODIGO_DEPARTAMENTO <- gsub("PETEN", 13, no_dup$CODIGO_DEPARTAMENTO)
# Quetzaltenango = 14
no_dup$CODIGO_DEPARTAMENTO <- gsub("QUETZALTENANGO", 14, no_dup$CODIGO_DEPARTAMENTO)
# Quiche = 15
no_dup$CODIGO_DEPARTAMENTO <- gsub("QUICHE", 15, no_dup$CODIGO_DEPARTAMENTO)
# Retaluleu = 16
no_dup$CODIGO_DEPARTAMENTO <- gsub("RETALULEU", 16, no_dup$CODIGO_DEPARTAMENTO)
# Sacatepequez = 17
no_dup$CODIGO_DEPARTAMENTO <- gsub("SACATEPEQUEZ", 17, no_dup$CODIGO_DEPARTAMENTO)
# San Marcos = 18
no_dup$CODIGO_DEPARTAMENTO <- gsub("SAN MARCOS", 18, no_dup$CODIGO_DEPARTAMENTO)
# Santa Rosa = 19
no_dup$CODIGO_DEPARTAMENTO <- gsub("SANTA ROSA", 19, no_dup$CODIGO_DEPARTAMENTO)
# Solola = 20
no_dup$CODIGO_DEPARTAMENTO <- gsub("SOLOLA", 20, no_dup$CODIGO_DEPARTAMENTO)
# Suchitepequez = 21
no_dup$CODIGO_DEPARTAMENTO <- gsub("SUCHITEPEQUEZ", 21, no_dup$CODIGO_DEPARTAMENTO)
# Totonicapan = 22
no_dup$CODIGO_DEPARTAMENTO <- gsub("TOTONICAPAN", 22, no_dup$CODIGO_DEPARTAMENTO)
# Zacapa = 23
no_dup$CODIGO_DEPARTAMENTO <- gsub("ZACAPA", 23, no_dup$CODIGO_DEPARTAMENTO)

# Eliminación de caracteres especiales

no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "[|/td>]", "")
no_dup$DIRECTOR <- str_replace(no_dup$DIRECTOR, "[|/td>]", "")
no_dup$DEPARTAMENTAL <- str_replace(no_dup$DEPARTAMENTAL, "[¼]", "")
no_dup$DEPARTAMENTAL <- str_replace(no_dup$DEPARTAMENTAL, "[t]", "")
no_dup$DEPARTAMENTAL <- str_replace(no_dup$DEPARTAMENTAL, "[d]", "")
no_dup$DEPARTAMENTAL <- str_replace(no_dup$DEPARTAMENTAL, "[/]", "")
no_dup$DEPARTAMENTAL <- str_replace(no_dup$DEPARTAMENTAL, "[>]", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "[¼]", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "[t]", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "[d]", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "[/]", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "[>]", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "[¼]", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "[t]", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "[d]", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "[/]", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "[>]", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "[;]", "")
no_dup$DIRECTOR <- str_replace(no_dup$DIRECTOR, "[¼]", "")
no_dup$DIRECTOR <- str_replace(no_dup$DIRECTOR, "[t]", "")
no_dup$DIRECTOR <- str_replace(no_dup$DIRECTOR, "[d]", "")
no_dup$DIRECTOR <- str_replace(no_dup$DIRECTOR, "[/]", "")
no_dup$DIRECTOR <- str_replace(no_dup$DIRECTOR, "[>]", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "#", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "%", "N")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "/", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "/", "")
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, "#", "")
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, '[""]', '')
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, '[""]', '')
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, '[""]', '')
no_dup$DIRECCION <- str_replace(no_dup$DIRECCION, '[""]', '')
no_dup$ESTABLECIMIENTO <- str_replace(no_dup$ESTABLECIMIENTO, "?", "")

