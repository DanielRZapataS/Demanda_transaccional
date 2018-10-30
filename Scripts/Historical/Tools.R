#-------INSTALACION DE PAQUETES-------

library(pacman) # paquete que carga paquetes y/o instala paquetes
pacman::p_load(stringr, # paquete para manipular strings
               data.table,
               stringi, # paquete para manipular strings
               lubridate, # manipular horas
               ggplot2, # gr?ficos
               gridExtra, # varias graficas en el mismo plot
               rlist) # manipulacion de listas


#------- FUNCIONES -------


# no permite la notacion cientifica
options(scipen = 999)

#esta funcion es la negacion de %in%
'%!in%' = Negate('%in%')

# INPUT
# file: archivo que quiero guardar
# directory: va entre comillas dobles y contiene la ruta donde quiero guardar la info
# filname: va entre comillas dobles y contiene el nombre del archivo con el que quiero guardar la data
# periodo: mes y año de la data
# OUTPUT
# no genera ningun output

Save_Dt <- function(dt, directory, filename, periodo){
  save <- paste0(directory, filename, periodo, ".csv")
  fwrite(dt, file = save, row.names = F)
}



# INPUT
# dt: data 
# var.den: variables de las que quiero obtener el porcentaje por fila de cada columna
# OUTPUT
# dt_result: data table de una sola columna con el %

Porc_Col <- function(dt, var.den){
  dt_result = NULL
  for(i in var.den){
    num <- dt[, i, with = F] #data.table de la variable i
    dt1 <- dt[, var.den, with = F] #data.table con las variables en var.den
    den <- rowSums(dt1, na.rm = T) #sumatoria de las filas
    dt2 <- num / den
    dt_result <- cbind(dt_result, dt2[[i]])
    #dt_result <- dt_result[, ':='(dt2 = dt2)]
  }
  dt_result <- as.data.table(dt_result)
  # pega la palabra ".PORC" a todas las palabras del vector
  var.den <- paste0(var.den, ".PORC")
  names(dt_result) <- var.den
  return(dt_result)
}


# Esta funcion hace un rbind de todas las bases de datos que guardan la misma 
# información pero con diferentes fechas
# INPUT
# path1: directorio donde se encuentran las bases de datos
# file.name: nombre en común de las bases de datos
# OUTPUT
# dt: base de datos que contiene la informacion de todas la bases de datos

compile_data <- function(path1, file.name){
  lista <- list.files(path = path1)
  casos_a_compilar <- paste0(file.name, months_compile, ".csv") 
  # imprimiendo warning en caso de que no exista alguna base de datos
  if(sum(casos_a_compilar %!in% lista) >= 1){
    x <- which(casos_a_compilar %!in% lista)
    message("Las bases que aparecen a continuacion no existen, por lo tanto no 
           se tienen en cuenta para la compilacion de la data")
    print(casos_a_compilar[x])
  }
  casos <- which(lista %in% casos_a_compilar)
  dt <- NULL
  for(i in casos){
    #print(i)
    a  <- fread(paste0(paste0(path1,"/"), lista[i]), header = T)#, 
    #colClasses = c(CNL_OFICINA = "character"))
    dt <- rbind(dt, a)
  }
  return(dt)
  gc()
}




#' upload files  
#' @return path of file searched 
get.path <- function(path, key_searcher){
  files <- list.files(path)
  searched_file <- grep(key_searcher, files, value = TRUE)
  searched_path <- os.path.join(path, searched_file)
  return(searched_path)
}



# Esta funcion crea un archivo con los nombres de las variables de la data,
# el tipo de variable y la longitud de la misma
# INPUT
# dt: data de la que se desea obtener la información
# OUTPUT
# metatVariables: tabla con los datos de las variables

meta <- function(dt){
  metatVariables <- data.table(variables = names(dt))
  metatVariables[, type := sapply(dt, class)]
  metatVariables[,length := sapply(dt, function(x){
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  return(metatVariables)
}



