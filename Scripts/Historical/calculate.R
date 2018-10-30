# calculate functions

calculate <- function(month_to_calculate){
  var1 = as.numeric(offices)
  dt <- get.path(original_path_hist, month_to_calculate) %>% fread(colClasses = "character")
  dt_cajeros <- get.path(dictionary_path_hist, "dicc_cajeros.csv") %>% fread(colClasses = "character")
  dt_parametros <- get.path(dictionary_path_hist, "parametros.csv") %>% fread(colClasses = "numeric")
  festivos <- get.path(dictionary_path_hist, "festivos.csv") %>% fread(colClasses = "Date")
  dt_extensiones_caj_avs <- get.path(dictionary_path_hist, "extensiones_caja_avs.csv") %>% fread(colClasses = "character")
  dicc_oficinas <- get.path(dictionary_path_hist, "OFICINAS CARTERA.csv") %>% fread(colClasses = "character")
  dicc <- get.path(dictionary_path_hist, "dicc_tx.csv") %>% fread(colClasses = "character")
  # function
  Funcion_Principal(dt,
                    dt_cajeros,
                    dt_parametros,
                    festivos,
                    dt_extensiones_caj_avs,
                    dicc_oficinas,
                    dicc,
                    var1)
}


calculate_per_month <- function(){
  lista <- list.files(path = original_path_hist)
  poss <- grep("DATOS_APLICATIVO_CANALES_", lista)
  casos_a_compilar <- lista[poss]
  for(i in months_cal){
    # imprimiendo warning en caso de que no exista alguna base de datos
    if(i %in% casos_a_compilar){
      calculate(i)
      gc()
    }else{
      message("Las bases que aparecen a continuación no existen, 
             por lo tanto no se calcula la informacion de esos periodos")
      print(i)
    }
  }
}



calculate_compile <- function(){
  data <- compile_data(month_out_path, "dt_frecu_cash_")
  data <- data[CAJ.AVS == "caja"]
  fwrite(data, file = paste0(out_path, "/dt_frecu_cash.csv"), row.names = F)
  fwrite(data, file = os.path.join(tablesFolder, "dt_frecu_cash.csv"))
  
  
  
  data <- compile_data(month_out_path, "dt_ind_ocp_")
  data <- data[CAJ.AVS == "caja"]
  fwrite(data, file = paste0(out_path, "/dt_ind_ocp.csv"), row.names = F)
  
  
  data <- compile_data(month_out_path, "dt_tabla_acumulada_")
  data <- data[CAJ.AVS == "caja"]
  fwrite(data, file = paste0(out_path, "/dt_tabla_acumulada.csv"), row.names = F)
  
  
  data <- compile_data(month_out_path, "dt_pronostico_")
  fwrite(data, file = paste0(out_path, "/dt_pronostico.csv"), row.names = F)
  fwrite(data, file = os.path.join(tablesFolder, "dt_pronostico.csv"))
  
  data <- compile_data(month_out_path, "dt_ind_departamento_")
  fwrite(data, file = paste0(out_path, "/dt_ind_departamento.csv"), row.names = F)
  
  
  data <- compile_data(month_out_path, "dt_ind_zona_")
  fwrite(data, file = paste0(out_path, "/dt_ind_zona.csv"), row.names = F)
  
  data <- compile_data(month_out_path, "dt_ind_jornada_")
  fwrite(data, file = paste0(out_path, "/dt_ind_jornada.csv"), row.names = F)
  
  data <- compile_data(month_out_path, "dt_ind_gerencia_")
  fwrite(data, file = paste0(out_path, "/dt_ind_gerencia.csv"), row.names = F)
  
}




rename <- function(){
  
  data <- fread(paste0(out_path, "/dt_ind_ocp.csv"), colClasses = "character")
  var.names <- c("Oficina",
                 "Jornada",
                 "Terminal",
                 "Ano-Mes",
                 "Llave",
                 "Tx>1",
                 "Tx<1",
                 "Otras Tx",
                 "Dias Mes",
                 "Tx Totales",
                 "Tx Esperadas",
                 "TO Hist",
                 "Nombre",
                 "Region",
                 "Zona",
                 "Departamento",
                 "Municipio",
                 "Gerencia administrativa")
  
  data[, ':='(CAJ.AVS = NULL,
              MAYORES_1.H.PICO = NULL,
              MENORES_1.H.PICO = NULL,
              OTROS.H.PICO = NULL,
              No_DIAS.H.PICO = NULL,
              No_Tx.H.PICO = NULL,
              Tx_ESP.H.PICO = NULL,
              PORC.OCUP.H.PICO = NULL,
              MAYORES_1.3H.PICO = NULL,
              MENORES_1.3H.PICO = NULL,
              OTROS.3H.PICO = NULL,
              No_DIAS.3H.PICO = NULL,
              No_Tx.3H.PICO = NULL,
              Tx_ESP.3H.PICO = NULL,
              PORC.OCUP.3H.PICO = NULL)]
  
  names(data) <- var.names
  data[, `TO Hist` := (as.numeric(`TO Hist`))/100]
  data[, ':='(Jornada = ifelse(Jornada == "J.EXT", "Adicional", "Normal"))]
  
  fwrite(data, file = os.path.join(tablesFolder, "dt_ind_ocp.csv"), row.names = F)
  
  metatVariables <-
    data.table(variables = names(data))
  metatVariables[, type := sapply(data, class)]
  metatVariables[, length := sapply(data, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariables,
    os.path.join(tablesFolder, "metatVariables_dt_ind_ocp.csv")
  )
  # segunda base
  
  data <- fread(paste0(out_path, "/dt_tabla_acumulada.csv"), colClasses = "character")
  var.names <- c("Oficina",
                 "Jornada",
                 "Nombre",
                 "Region",
                 "Zona",
                 "Departamento",
                 "Municipio",
                 "Ano-Mes",
                 "Llave",
                 "Gerencia administrativa",
                 "Tx>1",
                 "Tx<1",
                 "Otras Tx",
                 "Dias Mes",
                 "Tx Totales",
                 "Tx>1 Hora Pico",
                 "Tx<1 Hora Pico",
                 "Otras Tx Hora Pico",
                 "Tx Totales Hora Pico",
                 "Cajeros Hora Pico",
                 "Cajeros Planta",
                 "TO Hist",
                 "TO Hist Pico",
                 "Rango Ocupacion")
  
  data[, ':='(CAJ.AVS = NULL,
              MAYORES_1.3H.PICO = NULL,
              MENORES_1.3H.PICO = NULL,
              OTROS.3H.PICO = NULL,
              No_Tx.3H.PICO = NULL,
              No.CAJ.MAX = NULL,
              No.CAJ.H.PICO.MAX = NULL,
              No.CAJ.3H.PICO.MAX = NULL,
              PORC.OCUP.3H.PICO2 = NULL,
              No.CAJ.3H.PICO = NULL)]
  
  names(data) <- var.names
  data[, ':='(Jornada = ifelse(Jornada == "J.EXT", "Adicional", "Normal"))]
  fwrite(data, file = os.path.join(tablesFolder, "dt_tabla_acumulada.csv"), row.names = F)
  
  metatVariables <-
    data.table(variables = names(data))
  metatVariables[, type := sapply(data, class)]
  metatVariables[, length := sapply(data, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariables,
    os.path.join(tablesFolder, "metatVariables_dt_tabla_acumulada.csv")
  )
  
  # tercera base
  
  data <- fread(paste0(out_path, "/dt_ind_departamento.csv"), colClasses = "character")
  var.names <- c("Jornada",
                 "Departamento",
                 "Ano-Mes",
                 "Cantidad Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Cantidad Oficinas Tasa Ocupacion Mayor a 90%",
                 "Cantidad Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Porcentaje Oficinas Tasa Ocupacion Mayor a 90%")
  
  names(data) <- var.names
  data[, ':='(Jornada = ifelse(Jornada == "J.EXT", "Adicional", "Normal"))]
  fwrite(data, file = os.path.join(tablesFolder, "dt_ind_departamento.csv"), row.names = F)
  
  metatVariables <-
    data.table(variables = names(data))
  metatVariables[, type := sapply(data, class)]
  metatVariables[, length := sapply(data, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariables,
    os.path.join(tablesFolder, "metatVariables_dt_ind_departamento.csv")
  )
  
  # cuarta base
  
  data <- fread(paste0(out_path, "/dt_ind_zona.csv"), colClasses = "character")
  var.names <- c("Jornada",
                 "Zona",
                 "Ano-Mes",
                 "Cantidad Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Cantidad Oficinas Tasa Ocupacion Mayor a 90%",
                 "Cantidad Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Porcentaje Oficinas Tasa Ocupacion Mayor a 90%")
  
  names(data) <- var.names
  data[, ':='(Jornada = ifelse(Jornada == "J.EXT", "Adicional", "Normal"))]
  fwrite(data, file = os.path.join(tablesFolder, "dt_ind_zona.csv"), row.names = F)
  
  metatVariables <-
    data.table(variables = names(data))
  metatVariables[, type := sapply(data, class)]
  metatVariables[, length := sapply(data, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariables,
    os.path.join(tablesFolder, "metatVariables_dt_ind_zona.csv")
  )
  
  # Quinta base
  
  data <- fread(paste0(out_path, "/dt_ind_jornada.csv"), colClasses = "character")
  var.names <- c("Jornada",
                 "Ano-Mes",
                 "Cantidad Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Cantidad Oficinas Tasa Ocupacion Mayor a 90%",
                 "Cantidad Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Porcentaje Oficinas Tasa Ocupacion Mayor a 90%")
  
  names(data) <- var.names
  data[, ':='(Jornada = ifelse(Jornada == "J.EXT", "Adicional", "Normal"))]
  fwrite(data, file = os.path.join(tablesFolder, "dt_ind_jornada.csv"), row.names = F)
  
  metatVariables <-
    data.table(variables = names(data))
  metatVariables[, type := sapply(data, class)]
  metatVariables[, length := sapply(data, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariables,
    os.path.join(tablesFolder, "metatVariables_dt_ind_jornada.csv")
  )
  
  # sexta base
  
  data <- fread(paste0(out_path, "/dt_ind_gerencia.csv"), colClasses = "character")
  var.names <- c("Jornada",
                 "Gerencia administrativa",
                 "Ano-Mes",
                 "Cantidad Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Cantidad Oficinas Tasa Ocupacion Mayor a 90%",
                 "Cantidad Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Menor a 70%",
                 "Porcentaje Oficinas Tasa Ocupacion Entre 70% y 90%",
                 "Porcentaje Oficinas Tasa Ocupacion Mayor a 90%")
  
  names(data) <- var.names
  data[, ':='(Jornada = ifelse(Jornada == "J.EXT", "Adicional", "Normal"))]
  fwrite(data, file = os.path.join(tablesFolder, "dt_ind_gerencia.csv"), row.names = F)
  
  metatVariables <-
    data.table(variables = names(data))
  metatVariables[, type := sapply(data, class)]
  metatVariables[, length := sapply(data, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariables,
    os.path.join(tablesFolder, "metatVariables_dt_ind_gerencia.csv")
  )
  
  
}



















































































