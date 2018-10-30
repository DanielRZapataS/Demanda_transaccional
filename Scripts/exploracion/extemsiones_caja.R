## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999) 

library(data.table)
library(dplyr)
library(stringr)

source("Scripts/utilities.R")
source("Scripts/Forecast/file_tools.R")
# paths 
scriptPath <- "Scripts/exploracion"
datosPath <- "Data/Historical/Original"

# diccionario oficinas
extDictionary <- get.path(scriptPath, "diccionarioOficinas")[2] %>% fread()
extenciones <- grep("R", extDictionary$codigo, value = T)
extDictionary <- extDictionary[codigo %in% extenciones, .(codigo, codigoOficina)]

# cargue de datos 
files <- list.files(datosPath)
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
                    "Agosto","Septiembre","Octubre","Noviembre","Diciembre")
meses <- toupper(meses)
year <- c("2017", "2018")
searcher <- c(paste0(meses, year[1]), paste0(meses, year[2]))
dataOrganizer <- data.table(searcher = searcher)
dataOrganizer[, file := lapply(searcher, function(x){
  grep(x, files, value = T)})]
files <- unlist(dataOrganizer$file)
datos <- list()
i = files[12]
for( i in files[14:18]){
  data <- get.path(datosPath, i) %>% fread(colClasses = "character")
  if(i == 1){names.def <- names(data)}
  if(sum(which(names(data) == "CNL_PERIODO")) < 1){
    loc <- substr(i, 26, 370)
    yearLoc <- str_sub(loc, -4, -1 )
    mesLoc <- str_sub(loc, -100, -5 )
    mesLoc <- which(meses == mesLoc)
    if(length(mesLoc) < 2){    mesLoc <- paste0("0", mesLoc)}
    data[, CNL_PERIODO := as.numeric(paste0(yearLoc, mesLoc))]
    }
  data <- data[, .SD, .SDcols = names.def]
  data <- data[as.numeric(CNL_OFICINA) %in% extDictionary$codigoOficina]
  data <- data[, .N, by = .( CNL_OFICINA, CNL_PERIODO, CNL_TERMINAL  )]
  datos[[i]] <- data
}

datos1 <- rbindlist(datos)
fwrite(datos1, os.path.join(scriptPath, "extencionesTRX"))
datos1 <- fread(os.path.join(scriptPath, "extencionesTRX"))
setkey(datos1, CNL_OFICINA, CNL_PERIODO, N)
datos1

oficces <- unique(datos1$CNL_OFICINA)
datos1[CNL_PERIODO == 201808 & CNL_OFICINA == oficces[13], ]

data_aux <- data.table(names(summary(datos1$N)), datos1[, summary(N), CNL_OFICINA])

datos1[, mean := mean(N), by = .(CNL_OFICINA, CNL_PERIODO)]
datos3 <- datos1[N >  (mean-200)]
datos2 <- dcast(datos3, CNL_OFICINA + CNL_TERMINAL ~ CNL_PERIODO, value.var = "N")
datos2

fwrite(datos2, os.path.join(scriptPath, "extencionesTRX2.csv"))


##### script consulta una oficn a  

## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999) 

library(data.table)
library(dplyr)
library(stringr)

source("Scripts/utilities.R")
source("Scripts/Forecast/file_tools.R")
# paths 
scriptPath <- "Scripts/exploracion"
datosPath <- "Data/Historical/Original"

# diccionario oficinas
extDictionary <- get.path(scriptPath, "diccionarioOficinas")[2] %>% fread()
extenciones <- grep("R", extDictionary$codigo, value = T)
extDictionary <- extDictionary[codigo %in% extenciones, .(codigo, codigoOficina)]

# cargue de datos 
files <- list.files(datosPath)
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
           "Agosto","Septiembre","Octubre","Noviembre","Diciembre")
meses <- toupper(meses)
year <- c("2017", "2018")
searcher <- c(paste0(meses, year[1]), paste0(meses, year[2]))
dataOrganizer <- data.table(searcher = searcher)
dataOrganizer[, file := lapply(searcher, function(x){
  grep(x, files, value = T)})]
files <- unlist(dataOrganizer$file)
datos <- list()

i = files[1]
for( i in files){
  data <- get.path(datosPath, i) %>% fread(colClasses = "character")
  data <- data[ CNL_OFICINA == "084"]
  if(i == 1){names.def <- names(data)}
  if(sum(which(names(data) == "CNL_PERIODO")) < 1){
    loc <- substr(i, 26, 370)
    yearLoc <- str_sub(loc, -4, -1 )
    mesLoc <- str_sub(loc, -100, -5 )
    mesLoc <- which(meses == mesLoc)
    if(length(mesLoc) < 2){    mesLoc <- paste0("0", mesLoc)}
    data[, CNL_PERIODO := as.numeric(paste0(yearLoc, mesLoc))]
  }
  data <- data[, .N, by = .( CNL_OFICINA, CNL_PERIODO, CNL_TERMINAL  )]
  datos[[i]] <- data
}

datos1 <- rbindlist(datos, use.names = T)




