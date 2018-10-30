
################### PARA MAIN



Depura_Data <- function(var1, 
                        dt, 
                        dt_cajeros, 
                        dt_parametros,
                        festivos,
                        dt_extensiones_caj_avs,
                        dicc_oficinas,
                        dicc) {
  # organizando las variables
  dt[, ':='(TRANSACCION = CNL_SERV)]
  dt <-
    dt[, c(
      "CNL_USUARIO",
      "CNL_TERMINAL",
      "CNL_FECHA",
      "CNL_HORA",
      "TRANSACCION",
      "CNL_OFICINA",
      "CNL_ESTADO",
      "CNL_EFECTIVO",
      "CNL_CHEQUES",
      "CNL_TOTAL",
      "CNL_JORNADA"
    )]
  
  # diccionario con la información de transacciones de caja y avs
  # dicc <- fread("dat/dic/dicc_tx_james.csv",
  #               colClasses = "character")
  dicc[, ':='(TRANSACCION = Codigo_Acceso, NOMBRE.SERV = Servicio)]
  dicc <- dicc[!duplicated(dicc[, c("TRANSACCION")],
                           fromLast = T)]
  dicc <- dicc[, c("TRANSACCION", "NOMBRE.SERV")]
  
  
  # diccionario de oficinas
  # dicc_oficinas <- fread("dat/dic/OFICINAS CARTERA.csv")
  dicc_oficinas <- dicc_oficinas[, c(
    "CENT_COSTO",
    "NOMBRE",
    "REGION",
    "ZONA",
    "DEPARTAMENTO",
    "MUNICIPIO",
    "GERENCIA.ADMINISTRATIVA",
    "OFICINAS.ESTUDIO"
  )]
  
  # extensiones de caja y plataforma
  # dt_extensiones_caj_avs <-
  #   fread("dat/dic/extensiones_caja_avs.csv",
  #         colClasses = "character")
  
  # diccionario con DIAS festivos
  # festivos <- fread("dat/dic/festivos.csv",
  #                   colClasses = "Date",
  #                   header = T)
  
  # parametros de transacciones por hora y numero de horas laboradas
  # dt_parametros <- fread("dat/dic/parametros.csv")
  
  # numero de cajeros
  # dt_cajeros <- fread("dat/dic/dicc_cajeros.csv", colClasses = "character")
  dt_cajeros <- dt_cajeros[, c(
    "CNL_OFICINA",
    "CAJ.TOT",
    "CAJ.NOR",
    "CAJ.ADI",
    "CAJ.TOT.ESP",
    "CAJ.NOR.ESP",
    "CAJ.ADI.ESP"
  )]
  
  
  # todos los cod de tx tienen 4 digitos, encontes emparejo los codigos de tx
  # poniendo 0 al inicio de cada uno para hacer el cruce
  dicc[, ':='(TRANSACCION = str_pad(TRANSACCION, width = 4, 
                                    side = "left",
                                    pad = "0"))]
  
  dt <- merge(dt, dicc, by = "TRANSACCION", all.x = TRUE)
  
  
  # todos los cod de oficina tienen 3 digitos, entonces emparejo los codigos
  # poniendo 0 al inicio de cada uno para hacer el cruce
  dt_extensiones_caj_avs[, ':='(CNL_OFICINA = str_pad(CNL_OFICINA, width = 3, 
                                                      side = "left",
                                                      pad = "0"))]
  
  
  #identificando las extensiones de caja y avs
  dt <- merge(
    dt,
    dt_extensiones_caj_avs,
    by = c("CNL_TERMINAL", "CNL_OFICINA"),
    all.x = T
  )
  
  #calculando la variables TERMINAL. en esta se define si es cajero o avs
  dt[, ':='(l = nchar(CNL_TERMINAL))]
  dt[l %in% c(7, 8) &
       EXTENSION == "si", ':='(CNL_TERMINAL = CNL_TERMINAL2)]
  dt[, ':='(TERMINAL = stri_sub(CNL_TERMINAL,-2,-1))]
  
  
  # algunos elementos de la variable hora tienen 5 caracteres en lugar de 6
  # por esta razón emparejo la variable poniendo 0 a la izquierda
  dt[, ':='(CNL_HORA = str_pad(CNL_HORA, width = 6, 
                               side = "left",
                               pad = "0"))]
  
  # algunos elementos de la variable centro de costos tienen 2 caracteres en lugar
  # de 3 por esta razón emparejo la variable poniendo 0 a la izquierda
  dicc_oficinas[, ':='(CENT_COSTO = str_pad(CENT_COSTO, width = 3, 
                                            side = "left",
                                            pad = "0"))]
  
  # todos los cod de oficina tienen 3 digitos, entonces emparejo los codigos
  # poniendo 0 al inicio de cada uno para hacer el cruce
  dt_cajeros[, ':='(CNL_OFICINA = str_pad(CNL_OFICINA, width = 3, 
                                          side = "left",
                                          pad = "0"))]
  
  # creando la variable fecha y hora
  dt[, ':=' (FECHA_HORA = paste0(
    stri_sub(CNL_FECHA, 1, 10),
    sep = " ",
    stri_sub(CNL_HORA, 1, 2),
    sep = ":",
    stri_sub(CNL_HORA, 3, 4),
    sep = ":",
    stri_sub(CNL_HORA, 5, 6)
  ))]
  
  #selecciono los terminales de cajeros 01-19 y de avs 22 a 32
  term <- c(
    "01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15",
    "16",
    "17",
    "18",
    "19",
    "22",
    "23",
    "24",
    "25",
    "26",
    "27",
    "28",
    "29",
    "30",
    "31",
    "32"
  )
  
  # conviertiendo a numericos
  dt[, ':='(CNL_JORNADA = as.numeric(CNL_JORNADA),
            CNL_ESTADO = as.numeric(CNL_ESTADO))]
  #estado 1: Tx aprobadas, 3: Tx rechazadas
  estados  <- c(1, 3)
  transacciones <- dicc$TRANSACCION
  dt_master <- dt[TERMINAL %in% term]
  dt_master <- dt_master[CNL_ESTADO %in% estados]
  dt_master <- dt_master[TRANSACCION %in% transacciones]
  rm(dt, term, estados, transacciones)
  gc()
  
  
  #codificando las terminales de las extensiones
  dt_master[, ':='(CAJ.AVS = ifelse(as.numeric(TERMINAL) <= 19, "caja", "avs"))]
  dt_master_ext <- dt_master[EXTENSION == "si"]
  dt_master_ext[, ':='(TERMINAL = ifelse(CAJ.AVS == "caja", paste0(9, TERMINAL),
                                         paste0(8, TERMINAL)))]
  dt_master <- dt_master[is.na(EXTENSION) == T]
  dt_master <- rbind(dt_master, dt_master_ext)
  rm(dt_master_ext)
  gc()
  
  
  dt_master <-
    merge(
      dt_master,
      dicc_oficinas,
      by.x = "CNL_OFICINA",
      by.y = "CENT_COSTO" ,
      all.x = TRUE
    )
  
  # codificando nombres de oficina y codigos de oficina por extension
  # le agrega la palabra ext al final del codigo de la oficina
  # y le agrega ext al inicio del nombre de la oficina
  dt_master[is.na(EXTENSION) == T, ':='(EXTENSION = "no")]
  dt_master[, ':='(
    CNL_OFICINA = ifelse(EXTENSION == "si",
                         paste0(CNL_OFICINA, "ext"),
                         CNL_OFICINA),
    NOMBRE = ifelse(EXTENSION == "si",
                    paste0("ext", NOMBRE),
                    NOMBRE)
  )]
  
  
  # creando una llave que contiene la joranda de la oficina, el numero de la 
  # oficina y si se trata de oficina normal o extension de caja
  
  key.ext <- ifelse(grepl("ext", dt_master$CNL_OFICINA) == T, 1, 0)
  dt_master[, ':='(LLAVE = key.ext)]
  dt_master[, ':='(LLAVE = paste0(paste0(CNL_JORNADA, 
                                         gsub("ext", "", CNL_OFICINA)), 
                                  key.ext))]
  
  
  
  # calculando la variable fecha, DIA y HORARIO
  dt_master[, ':='(
    FECHA_HORA = as.POSIXct(FECHA_HORA, format = "%Y-%m-%d %H:%M:%S"),
    DIA = day(FECHA_HORA),
    DIAS.N = wday(FECHA_HORA),
    HORARIO = paste0(paste0(hour(FECHA_HORA), "-"), hour(FECHA_HORA) +
                       1)
  )]
  
  # calculando los DIAS de la semana
  dt_master[, ':='(DIAS = wday(FECHA_HORA))]
  dt_master <-
    dt_master[, ':='(
      #CNL_USUARIO = NULL,
      CNL_FECHA = NULL,
      CNL_HORA = NULL,
      CNL_ESTADO = NULL,
      V1 = NULL,
      EXTENSION = NULL
    )]
  
  # cambiando el nombre de la categoria de jornada
  dt_master[, ':='(CNL_JORNADA = as.factor(CNL_JORNADA))]
  levels(dt_master$CNL_JORNADA) <-
    sub("21", "J.NOR", levels(dt_master$CNL_JORNADA))
  levels(dt_master$CNL_JORNADA) <-
    sub("22", "J.EXT", levels(dt_master$CNL_JORNADA))
  
  #factorizando todos los horarios posibles de la base
  fhorario <-
    c(
      "5-6",
      "6-7",
      "7-8",
      "8-9",
      "9-10",
      "10-11",
      "11-12",
      "12-13",
      "13-14",
      "14-15",
      "15-16",
      "16-17",
      "17-18",
      "18-19",
      "19-20",
      "20-21",
      "21-22",
      "22-23"
    )
  dt_master <- dt_master[, HORARIO :=
                           factor(HORARIO, levels = fhorario,
                                  ordered = T)]
  rm(fhorario)
  
  # le quito los sábados y los domingos. 1:sunday, 2:monday, 3:tuesday
  dt_master <-
    dt_master[DIAS %in% c(2, 3, 4, 5, 6)]
  
  # le quito los DIAS festivos
  fest <- festivos$FESTIVOS
  dt_master <- dt_master[date(FECHA_HORA) %!in% as.Date(fest)]
  rm(fest)
  
  # si var1 es 1 solo trabajo con las oficinas identificadas con 1, las cuales
  # son las oficinas seleccionadas como casos prácticos para este estudio
  if (var1 == 1) {
    dt_master <- dt_master[OFICINAS.ESTUDIO == var1]
  } else{
    dt_master <- dt_master
  }
  
  # identificando las variables finales con las cuales voy a trabajar en mi
  # master table
  dt_master <- dt_master[, .(
    CNL_USUARIO,
    CNL_OFICINA,
    CNL_TERMINAL,
    TRANSACCION,
    CNL_EFECTIVO = as.numeric(CNL_EFECTIVO),
    CNL_CHEQUES = as.numeric(CNL_CHEQUES),
    CNL_TOTAL = as.numeric(CNL_TOTAL),
    CNL_JORNADA,
    TERMINAL,
    FECHA_HORA,
    CAJ.AVS,
    NOMBRE,
    REGION,
    ZONA,
    DEPARTAMENTO,
    MUNICIPIO,
    OFICINAS.ESTUDIO,
    DIA,
    HORARIO,
    DIAS,
    LLAVE,
    GERENCIA.ADMINISTRATIVA
  )]
  rm(var1)
  gc()
  lista_results <-
    list(
      dt_master = dt_master,
      dicc = dicc,
      dicc_oficinas = dicc_oficinas,
      dt_extensiones_caj_avs = dt_extensiones_caj_avs,
      festivos = festivos,
      dt_parametros = dt_parametros,
      dt_cajeros = dt_cajeros
    )
  return(lista_results)
}



# La funcion elimina las terminales que no alcanzaron un determinado numero
# de tx. El numero de tx minimo depende de un minimo de tx

# INPUT
# dt: master table
# tx.min: tx minimas por hora
# dt_par: data con los parametros del modelo
# OUTPUT
# dt: es le data master con terminales eliminadas
# dt_par: data con los parametros del numero de tx y horas trabajadas (dt_parametro)

Delete_Terminal <- function(dt, tx.min, dt_par) {
  dt1 <- dt[, .N, .(CNL_OFICINA, CNL_JORNADA, TERMINAL, DIA, CAJ.AVS)]
  dt1 <- dt1[, .(TX = sum(N),
                 DIAS.LABORADOS = length(DIA)),
             .(CNL_OFICINA, CNL_JORNADA, TERMINAL, CAJ.AVS)]
  dt1 <- dt1[CAJ.AVS == "caja"]
  # tx minimas esperadas or un cajero en un mes
  dt1[, ':='(
    TX.MIN = ifelse(
      CNL_JORNADA == "J.NOR",
      tx.min * DIAS.LABORADOS * dt_par$HORAS.NORM,
      tx.min * DIAS.LABORADOS * dt_par$HORAS.EXT
    )
  )]
  # terminales por oficina
  dt1[, ':='(N.TERM.OFI = uniqueN(TERMINAL)),
      .(CNL_OFICINA, CNL_JORNADA)]
  
  # marcando las terminales que debo eliminar por no cumplir con el minimo Tx
  # 1 para las que debo eliminar, 0 para las que debo dejar
  dt1[, ':='(ELIMINATE = ifelse(N.TERM.OFI == 1, 0, ifelse(TX >= TX.MIN, 0, 1)))]
  dt1[, ':='(
    TX = NULL,
    DIAS.LABORADOS = NULL,
    TX.MIN = NULL,
    N.TERM.OFI = NULL
  )]
  
  dt <- merge(
    dt,
    dt1,
    by = c("CNL_OFICINA", "CNL_JORNADA", "TERMINAL", "CAJ.AVS"),
    all.x = T
  )
  dt <- dt[ELIMINATE %in% c(0, NA)]
  dt[, ':='(ELIMINATE = NULL)]
  return(dt)
}

# dt_master <- Delete_Terminal(dt = dt_master, tx.min = 9, dt_par = dt_parametros)


# INPUT
# dt: data table del cual quiero obtener el mes y año con el que estoy trabajando
# OUTPUT
# periodo: mes y año de la dt


Perido_Mes <- function(dt)
{
  periodo <- dt[1, .(ano.mes = format(as.Date(FECHA_HORA), "%Y-%m"))]
  periodo <- as.character(periodo)
  return(periodo)
}

# mes y año de la data
# mes.ano <- Perido_Mes(dt = dt_master)



#--------------- calculando frecuencias



# INPUT:
# m: fecha de la data con la que se esta trabajando
# dt: tabla maestra
# var.agr: variables con las que se agrupa la tabla maestra
# var.sum: variables de agrupacion para obtener la suma y el promedio
# OUTPUT:
# dt_sum: tabla con las frecuencias medias de Tx, las tx
# totales por oficina 

Frecuency <- function(m, dt, var.agr, var.sum) {
  # frecuencia de Tx
  a <- dt[, .(N = .N), by = var.agr]
  # numero de tx al mes por oficina y numeo de dias por hora
  dt_sum <- a[, .(SUM.Tx = sum(N), N.DIAS = .N), by = var.sum]
  # dias maximos laborados en cada oficina
  var1 <- var.sum[var.sum != "HORARIO"]
  dt_sum[, ':='(MAX.DIAS = max(N.DIAS)), by = var1]
  # numero de tx promedio por oficina al dia
  dt_sum <- dt_sum[, ':='(MEAN.Tx = round(SUM.Tx / MAX.DIAS, 2))]
  dt_sum[, ':='(MAX.DIAS = NULL)]
  return(dt_sum)
}


# dt_frecu <-
#   Frecuency(
#     m = mes.ano,
#     dt = dt_master,
#     var.agr = c(
#       "CNL_OFICINA",
#       "CAJ.AVS",
#       "HORARIO",
#       "DIA",
#       "DEPARTAMENTO",
#       "MUNICIPIO",
#       "NOMBRE",
#       "REGION",
#       "ZONA",
#       "CNL_JORNADA"
#     ),
#     var.sum = c("CNL_OFICINA",
#                 "CAJ.AVS",
#                 "HORARIO",
#                 "DEPARTAMENTO",
#                 "MUNICIPIO",
#                 "NOMBRE",
#                 "REGION",
#                 "ZONA",
#                 "CNL_JORNADA")
#   )


# INPUT:
# m: fecha de la data con la que se esta trabajando
# dt: tabla maestra
# var.tra: variables con las que se agrupa el efectivo (la variable var.cast 
# tiene que estar incluida dentro de estas variables)
# trans: transacciones que no voy a tener en cuenta para el calculo del efectivo
# var.cast: variable con la que se realiza el cast
# OUTPUT:
# dt_efectivo: tabla con los montos que se mueven en efectivo agrupados
# por la variable var.tra


Cash <- function(m, dt, var.tra, trans, var.cast){
  # cantidad de efectivo que le entra a cada oficina
  # la tx 2360 corresponde al conteo de dinero en boveda
  var0 = c("CNL_EFECTIVO")
  var1 <- c(var.tra, var0)
  dt_efectivo <-
    dt[CAJ.AVS == "caja" & TRANSACCION %!in% trans & is.na(CNL_EFECTIVO) == F
       , var1, with = F]
  dt_efectivo <- dt_efectivo[, .(CNL_EFECTIVO = sum(CNL_EFECTIVO)),
                             by = var.tra]
  # codifico los valores de jornada
  dt_efectivo[, ':='(var.cast = as.factor(var.cast))]
  # los valores de jornada pasan a ser columnas
  var2 <- var1[var1 %!in% c(var0)]
  # Construyendo la formula
  f <- as.formula(paste0(paste(var2, collapse = " + "), "~", var.cast))
  # aplicando la formula en la fucnion dcast
  dt_efectivo <- dcast(
    dt_efectivo,
    f,
    value.var = var0
  )
  dt_efectivo[, ':='(MES = m, CAJ.AVS = "caja")]
  return(dt_efectivo)
}


# dt_cash <-
#   Cash(
#     m = mes.ano,
#     dt = dt_master,
#     var.tra = c(
#       "CNL_OFICINA",
#       "HORARIO",
#       "DEPARTAMENTO",
#       "MUNICIPIO",
#       "NOMBRE",
#       "REGION",
#       "ZONA",
#       "CNL_JORNADA"
#     ),
#     trans = c("2360"),
#     var.cast = c("CNL_JORNADA")
#   )

# INPUT
# dt: data master
# var.agr: variables por las cuales quiero detectar el numero de cajeros
# cond: variable condicional
# var.name: nombre de la variable de cajeros
# OUTPUT
# dt_No_caj: data con el numero de cajeros segun la agrupacion (var.agr)

N_Caj <- function(dt, var.agr, cond = "TERMINAL", var.name){
  # Numero de cajeros y avs por oficina max
  dt_No_caj <- dt[is.na(cond) == F, .(No.CAJ = uniqueN(TERMINAL)), by = var.agr]
  setnames(dt_No_caj, "No.CAJ", var.name)
  return(dt_No_caj)
}

# dt_caj <-
#   N_Caj(
#     dt = dt_master,
#     var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "DIA"),
#     var.name = "No.CAJ"
#   )





# INPUT
# dt1: data con las frecuencias de las transacciones
# dt2: data con el efectivo
# dt3: data con el numero de cajeros
# var.peg = variables en comun para pegar dt1 y dt2 
# var.peg2 = variables en comun para pegar la union de dt1 y dt2 con dt3
# m: mes de la data
# OUTPUT
# dt_result: data que resume la informacion de dt1, dt2 y dt3


Salida_Frec_Cash <- function(dt1, dt2, dt3, var.peg, var.peg2, m){
  dt_result <- merge(dt1,
                     dt2,
                     by = var.peg,
                     all.x = T)
  dt_result <- merge(dt_result,
                     dt3,
                     by = var.peg2,
                     all.x = T)
  dt_result[, ':='(MES = m)]
  return(dt_result)
}

# dt_frecu_cash <- Salida_Frec_Cash(
#   dt1 = dt_frecu,
#   dt2 = dt_cash,
#   dt3 = dt_caj,
#   var.peg = c(
#     "CNL_OFICINA",
#     "CAJ.AVS",
#     "HORARIO",
#     "DEPARTAMENTO",
#     "MUNICIPIO",
#     "NOMBRE",
#     "REGION",
#     "ZONA",
#     "CNL_JORNADA"
#   ),
#   var.peg2 = c(
#     "CNL_OFICINA",
#     "CAJ.AVS",
#     "CNL_JORNADA"
#   ),
#   m = mes.ano
# )





#-------IDENTIFICANDO LOS MAXIMOS DE CADA OFICINA -------

# INPUT
# dt: tabla a la que le quiero identificar las horas pico
# var.agr: variables de agrupacion para la identificacion del max 
# (IMPORTANTE: poner las variables en orden de tamaño de agrupacion por ejemplo:
# var.agr = c("CNL_OFICINA", "CNL_JORNADA"))
# OUTPUT
# dt: tabla con las horas pico identificadas

Iden_Max <- function(dt, var.agr){
  # calculo el No de Tx por horario, jornada, 
  # y oficina
  dt_result <- dt[, .(No_TX = .N),
                  by = var.agr]
  # ultimo elemento del vecto
  u <- var.agr[length(var.agr)]
  var1 <- var.agr[var.agr %!in% u]
  
  # ordenando por oficina, jornada y numero de transacciones de manera descendente
  unos <- rep(1, length(var1))
  dt_result <- setorderv(dt_result, c(var1, "No_TX"), c(unos,-1))
  
  # pongo numeros del 1:N, donde 1 es el maximo, 2 el segundo valor mas grande,
  # y 3 el tercer valor mas grande
  dt_result[, ':='(MAX.CAJ.AVS = seq(.N)), by = var1]
  
  
  dt <- merge(
    dt,
    dt_result,
    by = var.agr,
    all.x = T
  )
  return(dt)
}

# dt_master <-
#   Iden_Max(
#     dt = dt_master,
#     var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "HORARIO")
#   )




#-------TABLAS DE OCUPACION------------------------

# INPUT
# tipo (1, 2, 3): si es 1 se calcula el indice de ocupacion general
# si es 2 se calcula el indice de ocupacion de las horas pico
# si es 3 se calcula el indice de ocupacion de las 3 horas más congestionadas
# dt: tabla de la cual se quieren calcular las tasas de ocupacion
# var.agr: variables con las que se agrupa la tasa de ocupacion
# monto: monto por los cuales se quiere dividir las Tx
# var.cas: variables de agrupacion de las tasas de ocupacion
# parametros: parametros de tx
# m: mes y año de la data
# dicc: dt de dicc_con_extensiones
# OUTPUT
# dt_ind_ocupacion: devuelve una tabla con los indices de ocupacion

Tasas_Ocup <- function(tipo, dt, var.agr, monto, var.cas, parametros, m, dicc){
  
  if (tipo == 1) {
    dt_ind_ocupacion <- dt[, var.agr, with = F]
  } else if (tipo == 2) {
    dt_ind_ocupacion <- dt[MAX.CAJ.AVS == 1,
                           var.agr, with = F]
  } else{
    dt_ind_ocupacion <- dt[MAX.CAJ.AVS == 1 |
                             MAX.CAJ.AVS == 2 |
                             MAX.CAJ.AVS == 3,
                           var.agr, with = F]
  }
  
  # poninedo el valor en efectivo cuando en CNL_TOTAL esta vacio
  dt_ind_ocupacion[, ':='(CNL_TOTAL = ifelse (is.na(CNL_TOTAL), CNL_EFECTIVO, CNL_TOTAL))]
  # poninedo el valor en efectivo cuando esta vacio, cuando hago esto significa que
  # la tx es un retiro de dinero
  dt_ind_ocupacion[is.na(CNL_CHEQUES) | CNL_CHEQUES == 0,
                   ':='(CNL_EFECTIVO = ifelse (is.na(CNL_EFECTIVO) |
                                                 CNL_EFECTIVO == 0,
                                               CNL_TOTAL,
                                               CNL_EFECTIVO))]
  
  
  # divido el efectivo de la tx por montos, menores a 1mill = menores_1,
  # mayores a 1mill = mayores_1 y montos que no requieren efectivo = otros
  dt_ind_ocupacion[CNL_EFECTIVO > 0, ':='(MONTOS_Tx = ifelse(CNL_EFECTIVO <= monto,
                                                             "MENORES_1", "MAYORES_1"))]
  dt_ind_ocupacion[is.na(MONTOS_Tx), ':='(MONTOS_Tx = "OTROS")]
  
  # cuento el numero de tx por medio de la variable cont
  # mes de los datos se guarda en la variable m
  dt_ind_ocupacion[, ':='(CONT = 1)]
  # Construyendo la formula
  f <- as.formula(paste0(paste(var.cas, collapse = " + "), "~", "MONTOS_Tx"))
  
  dt_ind_ocupacion <- dcast(
    dt_ind_ocupacion,
    f,
    value.var = "CONT",
    fun = sum
  )
  
  # eliminando las tx por debajo de 9, esto con la idea de quitar los cajeros
  # que solo trabajaron 15 minutos en la hora pico
  dt_ind_ocupacion <- dt_ind_ocupacion[, ':='(tot = MAYORES_1 + MENORES_1 + OTROS)]
  dt_ind_ocupacion <- dt_ind_ocupacion[tot >= 9]
  
  dt_ind_ocupacion <- dt_ind_ocupacion[, .(
    MAYORES_1 = sum(MAYORES_1),
    MENORES_1 = sum(MENORES_1),
    OTROS = sum(OTROS)
  ),
  by = var.cas]
  
  # cuento el numero de dias por medio de la variable cont
  dt_ind_ocupacion[, ':='(CONT = 1)]
  var1 <- var.cas[var.cas %!in% "DIA"]
  dt_ind_ocupacion[, ':='(No_DIAS = sum(CONT)),
                   by = var1]
  
  dt_ind_ocupacion <-
    dt_ind_ocupacion[, .(
      MAYORES_1 = sum(MAYORES_1),
      MENORES_1 = sum(MENORES_1),
      OTROS = sum(OTROS),
      No_DIAS = max(No_DIAS)
    ),
    by = var1]
  
  # calculando el numero de Tx esperadas
  dt_ind_ocupacion[, ':='(No_Tx = MAYORES_1 + MENORES_1 + OTROS)]
  
  
  dt_ind_ocupacion[, ':='(
    PORC.MAY = MAYORES_1 / No_Tx,
    PORC.MEN = MENORES_1 / No_Tx,
    PORC.OTR = OTROS / No_Tx
  )]
  
  
  peso.nom <- names(parametros)
  dt_ind_ocupacion[, (peso.nom) := parametros]
  rm(peso.nom)
  
  
  # numero de horas trabajadas al mes
  if(tipo == 1){
    dt_ind_ocupacion[, ':='(HORAS.MES = ifelse (CNL_JORNADA == "J.NOR",
                                                HORAS.NORM * No_DIAS,
                                                HORAS.EXT  * No_DIAS))]
    
  }else if(tipo == 2){
    dt_ind_ocupacion[, ':='(HORAS.MES = ifelse (CNL_JORNADA == "J.NOR",
                                                1 * No_DIAS,
                                                1  * No_DIAS))]
    
  }else{
    dt_ind_ocupacion[, ':='(HORAS.MES = ifelse (CNL_JORNADA == "J.NOR",
                                                3 * No_DIAS,
                                                3  * No_DIAS))]
    
  }
  
  # horas distribuidas por el monto de la Tx
  dt_ind_ocupacion[, ':='(
    Tx_ESP = (HORAS.MES * PORC.MAY * PESO.MAY) +
      (HORAS.MES * PORC.MEN * PESO.MEN) +
      (HORAS.MES * PORC.OTR * PESO.OTR)
  )]
  
  # porcentaje de ocupación
  dt_ind_ocupacion[, ':='(PORC.OCUP = round(No_Tx * 100 / Tx_ESP , 2),
                          MES.ANO = m)]
  
  dt_ind_ocupacion <- merge(dt_ind_ocupacion,
                            dicc,
                            by = "CNL_OFICINA",
                            all.x = T)
  
  # eliminando variables
  variable.rm <- c(
    "PORC.MAY",
    "PORC.MEN",
    "PORC.OTR",
    "PESO.MAY",
    "PESO.MEN",
    "PESO.OTR",
    "HORAS.NORM",
    "HORAS.EXT",
    "HORAS.MES",
    "N"
  )
  
  dt_ind_ocupacion[, (variable.rm) := NULL]
  rm(variable.rm)
  
  
  if (tipo == 2) {
    setnames(dt_ind_ocupacion, "No_Tx", "No_Tx.H.PICO")
    setnames(dt_ind_ocupacion, "Tx_ESP", "Tx_ESP.H.PICO")
    setnames(dt_ind_ocupacion, "PORC.OCUP", "PORC.OCUP.H.PICO")
    setnames(dt_ind_ocupacion, "MAYORES_1", "MAYORES_1.H.PICO")
    setnames(dt_ind_ocupacion, "MENORES_1", "MENORES_1.H.PICO")
    setnames(dt_ind_ocupacion, "OTROS", "OTROS.H.PICO")
    setnames(dt_ind_ocupacion, "No_DIAS", "No_DIAS.H.PICO")
  } else if (tipo == 3) {
    setnames(dt_ind_ocupacion, "No_Tx", "No_Tx.3H.PICO")
    setnames(dt_ind_ocupacion, "Tx_ESP", "Tx_ESP.3H.PICO")
    setnames(dt_ind_ocupacion, "PORC.OCUP", "PORC.OCUP.3H.PICO")
    setnames(dt_ind_ocupacion, "MAYORES_1", "MAYORES_1.3H.PICO")
    setnames(dt_ind_ocupacion, "MENORES_1", "MENORES_1.3H.PICO")
    setnames(dt_ind_ocupacion, "OTROS", "OTROS.3H.PICO")
    setnames(dt_ind_ocupacion, "No_DIAS", "No_DIAS.3H.PICO")
  }
  return(dt_ind_ocupacion)
}



# # ind d ocupacion general
# dt_ind_ocup_gen <- Tasas_Ocup(
#   tipo = 1,
#   dt = dt_master,
#   var.agr = c(
#     "CNL_OFICINA",
#     "CNL_EFECTIVO",
#     "CNL_CHEQUES",
#     "CNL_TOTAL",
#     "CAJ.AVS",
#     "DIA",
#     "FECHA_HORA",
#     "TERMINAL",
#     "CNL_JORNADA"
#   ),
#   monto = 1000000,
#   var.cas = c("CNL_OFICINA",
#               "CAJ.AVS",
#               "TERMINAL",
#               "DIA",
#               "CNL_JORNADA"),
#   parametros = dt_parametros,
#   m = mes.ano,
#   dicc = dicc_con_extensiones
# )
# # ind d ocupacion horas pico
# dt_ind_ocup_pic <- Tasas_Ocup(
#   tipo = 2,
#   dt = dt_master,
#   var.agr = c(
#     "CNL_OFICINA",
#     "CNL_EFECTIVO",
#     "CNL_CHEQUES",
#     "CNL_TOTAL",
#     "CAJ.AVS",
#     "DIA",
#     "FECHA_HORA",
#     "TERMINAL",
#     "CNL_JORNADA"
#   ),
#   monto = 1000000,
#   var.cas = c("CNL_OFICINA",
#               "CAJ.AVS",
#               "TERMINAL",
#               "DIA",
#               "CNL_JORNADA"),
#   parametros = dt_parametros,
#   m = mes.ano,
#   dicc = dicc_con_extensiones
# )
# # ind d ocupacion 3 horas pico
# dt_ind_ocup_otr <- Tasas_Ocup(
#   tipo = 3,
#   dt = dt_master,
#   var.agr = c(
#     "CNL_OFICINA",
#     "CNL_EFECTIVO",
#     "CNL_CHEQUES",
#     "CNL_TOTAL",
#     "CAJ.AVS",
#     "DIA",
#     "FECHA_HORA",
#     "TERMINAL",
#     "CNL_JORNADA"
#   ),
#   monto = 1000000,
#   var.cas = c("CNL_OFICINA",
#               "CAJ.AVS",
#               "TERMINAL",
#               "DIA",
#               "CNL_JORNADA"),
#   parametros = dt_parametros,
#   m = mes.ano,
#   dicc = dicc_con_extensiones
# )


# INPUT
# dt1: data con el indice de ocupacion general
# dt2: data con el indice de ocupacion hora pico
# dt3: data con el indice de ocupacion 3 horas pico
# var.peg: variables para hacer el pegue de las bases 
# var.ide: variables de identificacion de la oficina
# OUTPUT
# tabla: data con la informacion del indice de ocupacion de los 3 conjuntos de datos


Merge_Ind_Ocup <- function(dt1, dt2, dt3, var.peg, var.ide){
  tabla <- merge(
    dt1,
    dt2[, (var.ide) := NULL],
    by = var.peg,
    all.x = T
  )
  
  tabla <- merge(
    tabla,
    dt3[, (var.ide) := NULL],
    by = var.peg,
    all.x = T
  )
  return(tabla)
}



# dt_tabla <- Merge_Ind_Ocup(
#   dt1 = dt_ind_ocup_gen,
#   dt2 = dt_ind_ocup_pic,
#   dt3 = dt_ind_ocup_otr,
#   var.peg = c("CNL_OFICINA", "CNL_JORNADA", "CAJ.AVS", "TERMINAL", "MES.ANO"),
#   var.ide = c("NOMBRE", "REGION", "ZONA", "DEPARTAMENTO", "MUNICIPIO")
# )


# 
# Save_Dt(
#   dt = dt_ind_ocup_gen,
#   directory = "out/out_mes/",
#   filename = "dt_ind_ocup_gen_",
#   periodo = mes.ano
# )
# 
# Save_Dt(
#   dt = dt_ind_ocup_gen,
#   directory = "out/out_mes/",
#   filename = "dt_ind_ocup_pic_",
#   periodo = mes.ano
# )
# 
# Save_Dt(
#   dt = dt_ind_ocup_gen,
#   directory = "out/out_mes/",
#   filename = "dt_ind_ocup_otr_",
#   periodo = mes.ano
# )
# 
# Save_Dt(
#   dt = dt_tabla,
#   directory = "out/out_mes/",
#   filename = "dt_ind_ocp_",
#   periodo = mes.ano
# )



# INPUT
# dt: data con los indices de ocupacion por cajero y avs
# var.agr: variables de agrupacion de la data
# OUTPUT
# dt_tabla_acumulada: data con los indices de ocupacion por oficina


Ind_Ocup_Acum <- function(dt, var.agr){
  dt_tabla_acumulada <- dt[, .(
    MAYORES_1 = sum(MAYORES_1),
    MENORES_1 = sum(MENORES_1),
    OTROS = sum(OTROS),
    No_DIAS = max(No_DIAS),
    No_Tx = sum(No_Tx),
    PORC.OCUP = mean(PORC.OCUP),
    SUM.PORC.OCUP = sum(PORC.OCUP, na.rm = T),
    MAYORES_1.H.PICO = sum(MAYORES_1.H.PICO, na.rm = T),
    MENORES_1.H.PICO = sum(MENORES_1.H.PICO, na.rm = T),
    OTROS.H.PICO = sum(OTROS.H.PICO, na.rm = T),
    No_Tx.H.PICO = sum(No_Tx.H.PICO, na.rm = T),
    PORC.OCUP.H.PICO = mean(PORC.OCUP.H.PICO, na.rm = T),
    SUM.PORC.OCUP.H.PICO = sum(PORC.OCUP.H.PICO, na.rm = T),
    MAYORES_1.3H.PICO = sum(MAYORES_1.3H.PICO, na.rm = T),
    MENORES_1.3H.PICO = sum(MENORES_1.3H.PICO, na.rm = T),
    OTROS.3H.PICO = sum(OTROS.3H.PICO, na.rm = T),
    No_Tx.3H.PICO = sum(No_Tx.3H.PICO, na.rm = T),
    PORC.OCUP.3H.PICO = mean(PORC.OCUP.3H.PICO, na.rm = T),
    SUM.PORC.OCUP.3H.PICO = sum(PORC.OCUP.3H.PICO, na.rm = T)
  ),
  by = var.agr]
}

# dt_tabla_acumulada <- Ind_Ocup_Acum(
#   dt = dt_tabla,
#   var.agr = c(
#     "CNL_OFICINA",
#     "CAJ.AVS",
#     "CNL_JORNADA",
#     "NOMBRE",
#     "REGION",
#     "ZONA",
#     "DEPARTAMENTO",
#     "MUNICIPIO",
#     "MES.ANO"
#   )
# )



# dt_caj_nor <-
#   N_Caj(dt = dt_master,
#         var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
#         var.name = "No.CAJ",
#         cond = "PORC.OCUP")
# 
# dt_caj_h_pico <-
#   N_Caj(dt = dt_master,
#         var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
#         var.name = "No.CAJ.H.PICO",
#         cond = "PORC.OCUP.H.PICO")
# 
# dt_caj_3h_pico <-
#   N_Caj(dt = dt_master,
#         var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
#         var.name = "No.CAJ.3H.PICO",
#         cond = "PORC.OCUP.3H.PICO")



# INPUT
# dt1: data con el numero de cajeros general
# dt2: data con el numero de cajeros en hora pico
# dt3: data con el numero de cajeros en las 3 horas pico
# var.peg: variables para realizar los pegues de la data
# var.caj: nombres de las variables de cajeros
# OUTPUT
# dt: data con la informacion del numero de cajeros de los 3 conjuntos de datos

Merge_No_Caj <- function(dt1, dt2, dt3, var.peg, var.caj){
  dt <- merge(dt1,
                  dt2,
                  by = var.peg,
                  all = T)
  dt <- merge(dt,
                  dt3,
                  by = var.peg,
                  all = T)
  
  # reemplazo NA por 0
  for (j in var.caj){
    set(dt, which(is.na(dt[[j]])),j,0)
  }

  dt <- dt[, (var.caj) := lapply(.SD, sum), 
           .SDcol = var.caj, 
           by = var.peg]

  # eliminando duplicados
  dt <- dt[!duplicated(dt[, ..var.peg], fromLast = T)]
  
  return(dt)
}


# dt_caj <- Merge_No_Caj(dt1 = dt_caj_nor,
#                        dt2 = dt_caj_h_pico,
#                        dt3 = dt_caj_3h_pico,
#                        var.peg = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
#                        var.caj = c("No.CAJ", "No.CAJ.H.PICO", "No.CAJ.3H.PICO"))
# 
# 

# INPUT
# dt: data master
# var.agr: variables de desagregacion
# OUTPUT
# dt1: data con el numero de dias en que estuvo activa la oficina



# # pegando data acumulada con el numero de cajeros
# 
# dt_tabla_acumulada <- merge(
#   dt_tabla_acumulada,
#   dt_caj,
#   by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
#   all.x = T
# )


# INPUT
# dt: data con con los indices de ocupacion por oficina, caj.avs y jornada 
# var: porcentaje de ocupacion que se quiere estimar con un 1 caj mas y sin 1 caj
# var.names: nombres de los nuevos porcentajes de ocupacion
# caj: nombre de la variable que contiene el numero de cajeros en el dt
# OUTPUT
# dt: data con los indices de ocupacion mas el numero de cajeros

Add_Cajeros <- function(dt, var, var.names, caj) {
  dt[,
     ':='(
       PPLUS = get(var) / (get(caj) + 1),
       PLESS = ifelse (
         get(caj) == 1,
         "No se pueden quitar mas cajeros",
         get(var) / (get(caj) - 1)
       )
     )]
  setnames(dt, "PPLUS", var.names[1])
  setnames(dt, "PLESS", var.names[2])
  return(dt)
}

# dt_tabla_acumulada <- Add_Cajeros(dt = dt_tabla_acumulada,
#                                   var = "SUM.PORC.OCUP",
#                                   var.names = c("PORC.OCUP.PLUS", "PORC.OCUP.LESS"),
#                                   caj = "No.CAJ"
#                                   )
# 
# dt_tabla_acumulada <- Add_Cajeros(dt = dt_tabla_acumulada,
#                                   var = "SUM.PORC.OCUP.H.PICO",
#                                   var.names = c("PORC.OCUP.PLUS.H.PICO", "PORC.OCUP.LESS.H.PICO"),
#                                   caj = "No.CAJ.H.PICO"
# )
# 
# dt_tabla_acumulada <- Add_Cajeros(dt = dt_tabla_acumulada,
#                                   var = "SUM.PORC.OCUP.3H.PICO",
#                                   var.names = c("PORC.OCUP.PLUS.3H.PICO", "PORC.OCUP.LESS.3H.PICO"),
#                                   caj = "No.CAJ.3H.PICO"
# )




# INPUT
# dt: data de la tabla acumulada (dt_tabla_acumulada)
# dt.para: data con los parametros de tx y de horas
# dt.porc.pron: data con los porcentajes de Tx por monto 
# var.int: variables que quiero traer de dt.porc.pron
# var.peg: variable en comun entre las bases a pegar, en este caso dt y dt.porc.pron
# var.names: nombres de los indices que esta calculando
# OUTPUT
# dt_tabla_acumulada: data con los indices de ocupacion por oficina



Ind_Ocup_Acum_Metodo2 <- function(dt, 
                                  dt.para, 
                                  dt.porc.pron,
                                  var.int,
                                  var.names){
  
  dt <- dt[, .(
    MAYORES_1 = sum(MAYORES_1),
    MENORES_1 = sum(MENORES_1),
    OTROS = sum(OTROS),
    No_DIAS = max(No_DIAS),
    No_Tx = sum(No_Tx, na.rm = T),
    PORC.OCUP = mean(PORC.OCUP),
    MAYORES_1.H.PICO = sum(MAYORES_1.H.PICO, na.rm = T),
    MENORES_1.H.PICO = sum(MENORES_1.H.PICO, na.rm = T),
    OTROS.H.PICO = sum(OTROS.H.PICO, na.rm = T),
    No_DIAS.H.PICO = mean(No_DIAS.H.PICO, na.rm = T),
    No_Tx.H.PICO = sum(No_Tx.H.PICO, na.rm = T),
    PORC.OCUP.H.PICO = mean(PORC.OCUP.H.PICO, na.rm = T),
    MAYORES_1.3H.PICO = sum(MAYORES_1.3H.PICO, na.rm = T),
    MENORES_1.3H.PICO = sum(MENORES_1.3H.PICO, na.rm = T),
    OTROS.3H.PICO = sum(OTROS.3H.PICO, na.rm = T),
    No_DIAS.3H.PICO = mean(No_DIAS.3H.PICO, na.rm = T),
    No_Tx.3H.PICO = sum(No_Tx.3H.PICO, na.rm = T),
    PORC.OCUP.3H.PICO = mean(PORC.OCUP.3H.PICO, na.rm = T)
  ),
  by = c("CNL_OFICINA", "CNL_JORNADA", "CAJ.AVS", "LLAVE")]
  
  
  dt <- dt[CAJ.AVS == "caja"]
  dt[, ':='(CAJ.AVS = NULL)]
  
  # agregando el peso de los parametros a la data
  peso.nom <- names(dt.para)
  dt[, (peso.nom) := dt.para]
  rm(peso.nom)
  
  # agregando los % de los montos a la data y los cajeros
  dt <- merge(dt,
              dt.porc.pron[, var.int, with = F],
              by = "LLAVE",
              all.x = T)
  
  # poniendo 0 cuando hay NA en el numero de cajeros
  nombres <- names(dt)
  pos <- which(nombres %in% c("No.CAJ", "No.CAJ.H.PICO", "No.CAJ.3H.PICO"))
  
  # reemplazo inf por 0
  for (j in pos){
    set(dt, which(!is.finite(dt[[j]])),j,0)
  }
  

  
  # identificando los nombres de las variables que necesito para hacer el calculo
  # identificando las variables que manejan montos de Tx
  var1 <- names(dt)
  var.nor <- c(grep("PORC$", var1, value = TRUE))
  
  # organizando las tripletas (nombres de las variables)
  var2.nor <- list(var.nor[1:3],
                   var.nor[4:6],
                   var.nor[7:9])
  
  # calculando las tasas de ocupacion "metodologia 2"
  
  var3 <- var2.nor
  n <- length(var3)
  dt4 = NULL
  
  for(i in 1:n){
    dt2 <- dt[, var3[[i]], with = F]
    dt3 <- dt[, .(PESO.MAY, PESO.MEN, PESO.OTR)]
    tx <- ((dt2[,1] * dt3[,1]) + (dt2[,2] * dt3[,2]) + (dt2[,3] * dt3[,3]))
    if(i %in% c(1)){ # variables para todo el dia
      dias.horas.caj <- dt[, .(No.CAJ)] * dt[, .(HORAS.NORM)] * dt[, .(No_DIAS)]
    }else if(i %in% c(2)){ # variables para horas pico
      dias.horas.caj <- dt[, .(No.CAJ.H.PICO)] * dt[, .(No_DIAS.H.PICO)]
    }else{ # variables para 3 horas pico
      dias.horas.caj <- dt[, .(No.CAJ.3H.PICO)] * dt[, .(HORAS.EXT)] * dt[, .(No_DIAS.3H.PICO)]
    }
    # denominador: tx esperadas
    den <- tx * dias.horas.caj
    # numerador: tx del mes
    if(i %in% c(1)){
      num <- dt[, .(No_Tx)]
    }else if(i %in% c(2)){
      num <- dt[, .(No_Tx.H.PICO)]
    }else{
      num <- dt[, .(No_Tx.3H.PICO)]
    }
    
    tasa.ofi <- num / den
    # dt4 guarda cada tasa.ofi
    dt4 <- cbind(dt4, tasa.ofi[[1]])
  }
  
  dt4 <- as.data.table(dt4)
  # reemplazo inf por 0
  for (j in names(dt4)){
    # otra manera de remplazar los valores
    # dt4[[j]][which(!is.finite(dt4[[j]]))] <- 0 
    set(dt4, which(!is.finite(dt4[[j]])),j,0)
  }
  
  dt <- cbind(dt, dt4) # pegando los indicadores a la base
  
  setnames(dt, c("V1", "V2", "V3"), var.names)
  
  var6 <- c("CNL_OFICINA", "CNL_JORNADA", var.names)
  
  dt <- dt[, var6, with = F]
  dt[, ':='(CAJ.AVS = "caja")]
}



# dt_ind_met2 <- Ind_Ocup_Acum_Metodo2(
#   dt = dt_tabla,
#   dt.para = dt_parametros,
#   dt.porc.pron = dt_pronostico,
#   var.int = c("LLAVE",
#               "MAYORES_1.PORC",
#               "MENORES_1.PORC",
#               "OTROS.PORC",
#               "MAYORES_1.H.PICO.PORC",
#               "MENORES_1.H.PICO.PORC",
#               "OTROS.H.PICO.PORC",
#               "MAYORES_1.3H.PICO.PORC",
#               "MENORES_1.3H.PICO.PORC",
#               "OTROS.3H.PICO.PORC",
#               "No.CAJ",
#               "No.CAJ.H.PICO",
#               "No.CAJ.3H.PICO"),
#   var.names = c("PORC.OCUP2",
#                 "PORC.OCUP.H.PICO2",
#                 "PORC.OCUP.3H.PICO2")
# )





# INPUT
# dt: data master
# caj.avs: ("caja", "avs")
# var.agr: variables de agrupacion de la data
# var.cast: variable para la que se quiere obtener el porcentaje
# m: mes y año de la data
# OUTPUT
# tabla: contiene los porcentajes Tx de cada oficina por dia de la semana 
# (Lunes, Martes, Miercoles, Jueves y Viernes)



Porc_Jornada <- function(dt, caj.avs, var.agr, var.cast, m) {
  porcentajes <- dt[CAJ.AVS == caj.avs,
                    .N,
                    by = var.agr]
  var2 <- var.agr[var.agr %!in% c(var.cast)]
  # calculando los porcentajes
  porcentajes[, porc := N / sum(N), by = var2]
  # Construyendo la formula
  f <- as.formula(paste0(paste(var2, collapse = " + "), "~", var.cast))
  # aplicando la formula en la fucnion dcast  
  tabla <- dcast(
    porcentajes,
    f,
    value.var = "porc",
    fill = 0
  )
  tabla[, ':='(MES = m)]
  return(tabla)
}

# dt_porcentajes <- Porc_Jornada(dt = dt_master,
#                                caj.avs = "caja",
#                                var.agr = c("CNL_JORNADA", "CNL_OFICINA", "DIAS.N"),
#                                var.cast = "CNL_JORNADA",
#                                m = mes.ano)

# Save_Dt(
#   dt = dt_porcentajes,
#   directory = "out/out_mes/",
#   filename = "dt_porcentajes_",
#   periodo = mes.ano
# )



# agregando el indice de ocupacion a la tabla de frecuencias

# dt_frecu_cash <- merge(
#   dt_frecu_cash,
#   dt_tabla_acumulada[, .(CNL_OFICINA,
#                          CAJ.AVS,
#                          CNL_JORNADA ,
#                          PORC.OCUP,
#                          PORC.OCUP.H.PICO,
#                          PORC.OCUP.3H.PICO)],
#   by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
#   all.x = T
# )


# Save_Dt(
#   dt = dt_frecu_cash,
#   directory = "out/out_mes/",
#   filename = "dt_frecu_cash_",
#   periodo = mes.ano
# )




#------------- cambiando el  --------------

# dt: tablas con las tasas de ocupacion acumuladas por oficina
# var.null: variables que no son necesarias en la data
# var.cast: variable que se quiere cambiar de categoria de fila a columna independiente
# var.agr: variables de agrupacion
# var.comb: variables que se quiere transponer (var.cast)

Porc_Pronostico <- function(dt, var.null, var.cast, var.agr, var.comb){
  
  dt <- dt[CAJ.AVS == "caja"]
  
  # eliminando variables
  dt[, (var.null) := NULL]
  
  
  # f <- as.formula(paste0(paste(var.agr, collapse = " + "), "~", var.cast))
  # tabla <-
  #   dcast(
  #     dt,
  #     f,
  #     value.var = var.comb,
  #     fill = 0,
  #     fun.aggregate = sum
  #   )

  var1 <- names(dt)
  
  # identificando las variables que manejan montos de Tx
  var2 <- c(grep("^MAY", var1, value = TRUE), 
            grep("^MEN", var1, value = TRUE),
            grep("^OTR", var1, value = TRUE))
  # guardando los nombres de las variables por similaridad
  var3 <- list()
  n <- length(var2)/3
  for(i in 1:n){
    var3 <- list.append(var3, c(var2[i], var2[i+3], var2[i+6]))
  }
  
  # calculando los % de Tx segun el monto
  dt1 <- list()
  n <- length(var3)
  for(i in 1:n){
    dt2 <- Porc_Col(
      dt = dt,
      var.den = var3[[i]]
      )
    dt1 <- list.append(dt1, dt2)
  }
  
  # adicionando la informacion de los % a la dt
  for(i in 1:n){
    dt <- cbind(dt, dt1[[i]])
  }
  
  var2.porc <- paste0(var2, ".PORC")
  
  # reemplazo nan por 0
  for (j in var2.porc){
    set(dt, which(is.nan(dt[[j]])),j,0)
  }
  return(dt)
}

# dt_pronostico <-
#   Porc_Pronostico(
#     dt = dt_tabla_acumulada,
#     var.null = c(
#       "No_DIAS",
#       "CAJ.AVS",
#       "PORC.OCUP",
#       "SUM.PORC.OCUP",
#       "PORC.OCUP.PLUS",
#       "PORC.OCUP.LESS",
#       "PORC.OCUP.H.PICO",
#       "SUM.PORC.OCUP.H.PICO",
#       "PORC.OCUP.PLUS.H.PICO",
#       "PORC.OCUP.LESS.H.PICO",
#       "PORC.OCUP.3H.PICO",
#       "SUM.PORC.OCUP.3H.PICO",
#       "PORC.OCUP.PLUS.3H.PICO",
#       "PORC.OCUP.LESS.3H.PICO"
#     ),
#     var.cast = "CNL_JORNADA",
#     var.agr = c(
#       "CNL_OFICINA",
#       "NOMBRE",
#       "REGION",
#       "ZONA",
#       "DEPARTAMENTO",
#       "MUNICIPIO",
#       "MES.ANO",
#       "LLAVE"
#     ),
#     var.comb = c(
#       "MAYORES_1",
#       "MENORES_1",
#       "OTROS",
#       "No_Tx",
#       "No.CAJ",
#       "MAYORES_1.H.PICO",
#       "MENORES_1.H.PICO",
#       "OTROS.H.PICO",
#       "No_Tx.H.PICO",
#       "No.CAJ.H.PICO",
#       "MAYORES_1.3H.PICO",
#       "MENORES_1.3H.PICO",
#       "OTROS.3H.PICO",
#       "No_Tx.3H.PICO",
#       "No.CAJ.3H.PICO"
#     )
#   )
# 
# 
# 
# 
# Save_Dt(
#   dt = dt_pronostico,
#   directory = "out/out_mes/",
#   filename = "dt_pronostico_",
#   periodo = mes.ano
# )




# calcula el numero y el porcentaje de oficinal que tienen un porcentaje de ocupacion
# por debajo del 70, entre el 70 y el 90, y por encima del 90 porciento
# INPUT
# dt: base de datos que se quiere agrupar
# var.1: variables con las cuales se piensa agrupar la data
# var.2: variables de las que se quiere conocer el porcentaje
# de oficinas que tiene su porcentaje de ocupacion entre 70 y 90
# OUTPUT
# dt_result: data con los valores de cada clase y porcentajes
# 1: porcentaje de ocupacion por debajo de 70
# 2: porcentaje de ocupacion entre 70 y 90
# 3: porcentaje de ocupacion por encima de 90

Count_Porc_Ocupacion <- function(dt, var.1) {
  # agregando los valores
  dt[, ':='(RANG.PORC = ifelse(PORC.OCUP2 < 0.7, "men a 70%",
                          ifelse(PORC.OCUP2 <= 0.9, "ent 70% y 90%", "mas del 90%")))]
  # Construyendo la formula
  f <- as.formula(paste0(paste(var.1, collapse = " + "), "~", "RANG.PORC"))
  var.1 <- c(var.1, "RANG.PORC")
  dt <- dt[CAJ.AVS == "caja", .N, by = var.1]
  # aplicando la formula en la fucnion dcast
  dt <- dcast.data.table(
    dt,
    f,
    fill = 0,
    value.var = "N",
    fun = sum
  )
  # porcentaje de las columnas
  dt1 <- Porc_Col(dt, c("men a 70%", "ent 70% y 90%", "mas del 90%"))
  dt <- cbind(dt, dt1)
  dt_result <- dt
  return(dt_result)
}

# dt1 <- Count_Porc_Ocupacion(dt = dt_tabla_acumulada,
#                             var.1 =
#                               c("CNL_JORNADA", "DEPARTAMENTO", "MES.ANO"))



#data <- fread("dat/org/DATOS_APLICATIVO_CANALES_ENERO2018", colClasses = "character")

# dt: data del mes
# pronósticos

Funcion_Principal <- function(dt,
                              dt_cajeros,
                              dt_parametros,
                              festivos,
                              dt_extensiones_caj_avs,
                              dicc_oficinas,
                              dicc,
                              var1){
  
#Funcion_Principal <- function(){
  
  
  
  lista.results <- Depura_Data(var1, 
                               dt, 
                               dt_cajeros, 
                               dt_parametros,
                               festivos,
                               dt_extensiones_caj_avs,
                               dicc_oficinas,
                               dicc)
  
  # poniendo la data depurada
  dt_master <- lista.results$dt_master
  dicc <- lista.results$dicc
  dicc_oficinas <- lista.results$dicc_oficinas
  dt_extensiones_caj_avs <- lista.results$dt_extensiones_caj_avs
  festivos <- lista.results$festivos
  dt_parametros <- lista.results$dt_parametros
  dt_cajeros <- lista.results$dt_cajeros
  
  # diccionario de oficinas con codigos de oficina de extensiones
  dicc_con_extensiones <-
    dt_master[, .N, by = .(CNL_OFICINA, 
                           NOMBRE, 
                           REGION, 
                           ZONA, 
                           DEPARTAMENTO, 
                           MUNICIPIO,
                           GERENCIA.ADMINISTRATIVA)]
  
  rm(lista.results)
  gc()
  
  #return(dt_master)
  
  dt_master <- Delete_Terminal(dt = dt_master, tx.min = 9, dt_par = dt_parametros)
  mes.ano <- Perido_Mes(dt = dt_master)
  
  
  # guardando el data master
  Save_Dt(
    dt = dt_master,
    #directory = "dat/sta/",
    directory = staging_path_hist,
    filename = "/dt_master_",
    periodo = mes.ano
  )
  
  # calculando la frecuencia de las transacciones
  # m: fecha de la data con la que se esta trabajando
  # dt: tabla maestra
  # var.agr: variables con las que se agrupa la tabla maestra
  # var.sum: variables de agrupacion para obtener la suma y el promedio
  dt_frecu <-
    Frecuency(
      m = mes.ano,
      dt = dt_master,
      var.agr = c(
        "CNL_OFICINA",
        "CAJ.AVS",
        "HORARIO",
        "DIA",
        "DEPARTAMENTO",
        "MUNICIPIO",
        "NOMBRE",
        "REGION",
        "ZONA",
        "CNL_JORNADA",
        "LLAVE"
      ),
      var.sum = c("CNL_OFICINA",
                  "CAJ.AVS",
                  "HORARIO",
                  "DEPARTAMENTO",
                  "MUNICIPIO",
                  "NOMBRE",
                  "REGION",
                  "ZONA",
                  "CNL_JORNADA",
                  "LLAVE")
    )
  
  
  # Calculando el efectivo de las Tx en caja
  # m: fecha de la data con la que se esta trabajando
  # dt: tabla maestra
  # var.tra: variables con las que se agrupa el efectivo (la variable var.cast 
  # tiene que estar incluida dentro de estas variables)
  # trans: transacciones que no voy a tener en cuenta para el calculo del efectivo
  # var.cast: variable con la que se realiza el cast
  
  dt_cash <-
    Cash(
      m = mes.ano,
      dt = dt_master,
      var.tra = c(
        "CNL_OFICINA",
        "HORARIO",
        "DEPARTAMENTO",
        "MUNICIPIO",
        "NOMBRE",
        "REGION",
        "ZONA",
        "CNL_JORNADA"
      ),
      trans = c("2360"),
      var.cast = c("CNL_JORNADA")
    )
  
  # calcula el numero de cajeros por dia del mes
  # calculando el numero de cajeros
  # dt: data master
  # var.agr: variables por las cuales quiero detectar el numero de cajeros
  # cond: variable condicional
  # var.name: nombre de la variable de cajeros
  
  # dt_caj <-
  #   N_Caj(
  #     dt = dt_master,
  #     var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "DIA", "HORARIO"),
  #     var.name = "No.CAJ"
  #   )
  # 
  # # ahora calculo el numero de cajeros por oficina, jornada y hora
  # dt_caj <- dt_caj[, .(No.CAJ = round(mean(No.CAJ), 2)), 
  #                  by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "HORARIO")]
  
  
  
  # obtengo el número de tx por terminal
  dt_prov <-
    dt_master[CAJ.AVS == "caja", .N, .(CNL_OFICINA, CNL_JORNADA, CAJ.AVS, DIA, HORARIO, TERMINAL)]
  # elimino los cajero que solo trabajaron 15 minutos en esa hora, es decir
  # los que tienen 9 tx o menos
  dt_prov <-  dt_prov[N >= 9]
  
  dt_caj_h <-
    N_Caj(
      dt = dt_prov,
      var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "DIA", "HORARIO"),
      var.name = "No.CAJ.H"
    )
  
  # ahora calculo el numero de cajeros promedio  por mes
  dt_caj <-
    dt_caj_h[, .(No.CAJ.H = round(mean(No.CAJ.H), 2)),
                  by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "HORARIO")]
  
  rm(dt_caj_h)
  
  # pegando la informacion de transacciones, numero de cajeros y montos en efectivo
  # base de datos con la frecuencia de las Tx, el efectivo y numero de cajeros
  # dt: data master
  # dt1: data con las frecuencias de las transacciones
  # dt2: data con el efectivo
  # dt3: data con el numero de cajeros
  # var.peg = variables en comun para pegar dt1 y dt2 
  # var.peg2 = variables en comun para pegar la union de dt1 y dt2 con dt3
  # m: mes de la data
  
  dt_frecu_cash <- Salida_Frec_Cash(
    dt1 = dt_frecu,
    dt2 = dt_cash,
    dt3 = dt_caj,
    var.peg = c(
      "CNL_OFICINA",
      "CAJ.AVS",
      "HORARIO",
      "DEPARTAMENTO",
      "MUNICIPIO",
      "NOMBRE",
      "REGION",
      "ZONA",
      "CNL_JORNADA"
    ),
    var.peg2 = c(
      "CNL_OFICINA",
      "CAJ.AVS",
      "CNL_JORNADA",
      "HORARIO"
    ),
    m = mes.ano
  )
  
  rm(dt_caj, dt_cash, dt_frecu)
  gc()
  
  # identificando los maximos de las oficinas segun su horario
  # dt: tabla a la que le quiero identificar las horas pico
  # var.agr: variables de agrupacion para la identificacion del max 
  # (IMPORTANTE: poner las variables en orden de tamaño de agrupacion por ejemplo:
  # var.agr = c("CNL_OFICINA", "CNL_JORNADA"))
  
  dt_master <-
    Iden_Max(
      dt = dt_master,
      var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "HORARIO")
    )
  
  # calculando los indices de ocupacion 
  # tipo (1, 2, 3): si es 1 se calcula el indice de ocupacion general
  # si es 2 se calcula el indice de ocupacion de las horas pico
  # si es 3 se calcula el indice de ocupacion de las 3 horas más congestionadas
  # dt: tabla de la cual se quieren calcular las tasas de ocupacion
  # var.agr: variables con las que se agrupa la tasa de ocupacion
  # monto: mosnto por los cuales se quiere dividir las Tx
  # var.cas: variables de agrupacion de las tasas de ocupacion
  
  
  # ind de ocupacion general
  dt_ind_ocup_gen <- Tasas_Ocup(
    tipo = 1,
    dt = dt_master,
    var.agr = c(
      "CNL_OFICINA",
      "CNL_EFECTIVO",
      "CNL_CHEQUES",
      "CNL_TOTAL",
      "CAJ.AVS",
      "DIA",
      "FECHA_HORA",
      "TERMINAL",
      "CNL_JORNADA",
      "LLAVE"
    ),
    monto = 1000000,
    var.cas = c("CNL_OFICINA",
                "CAJ.AVS",
                "TERMINAL",
                "DIA",
                "CNL_JORNADA",
                "LLAVE"),
    parametros = dt_parametros,
    m = mes.ano,
    dicc = dicc_con_extensiones
  )
  # ind d ocupacion horas pico
  dt_ind_ocup_pic <- Tasas_Ocup(
    tipo = 2,
    dt = dt_master,
    var.agr = c(
      "CNL_OFICINA",
      "CNL_EFECTIVO",
      "CNL_CHEQUES",
      "CNL_TOTAL",
      "CAJ.AVS",
      "DIA",
      "FECHA_HORA",
      "TERMINAL",
      "CNL_JORNADA",
      "LLAVE"
    ),
    monto = 1000000,
    var.cas = c("CNL_OFICINA",
                "CAJ.AVS",
                "TERMINAL",
                "DIA",
                "CNL_JORNADA",
                "LLAVE"),
    parametros = dt_parametros,
    m = mes.ano,
    dicc = dicc_con_extensiones
  )
  # ind d ocupacion 3 horas pico
  dt_ind_ocup_otr <- Tasas_Ocup(
    tipo = 3,
    dt = dt_master,
    var.agr = c(
      "CNL_OFICINA",
      "CNL_EFECTIVO",
      "CNL_CHEQUES",
      "CNL_TOTAL",
      "CAJ.AVS",
      "DIA",
      "FECHA_HORA",
      "TERMINAL",
      "CNL_JORNADA",
      "LLAVE"
    ),
    monto = 1000000,
    var.cas = c("CNL_OFICINA",
                "CAJ.AVS",
                "TERMINAL",
                "DIA",
                "CNL_JORNADA",
                "LLAVE"),
    parametros = dt_parametros,
    m = mes.ano,
    dicc = dicc_con_extensiones
  )
  
  # pegando los indices de ocupacion
  # dt1: data con el indice de ocupacion general
  # dt2: data con el indice de ocupacion hora pico
  # dt3: data con el indice de ocupacion 3 horas pico
  # var.peg: variables para hacer el pegue de las bases 
  # var.ide: variables de identificacion de la oficina
  
  dt_tabla <- Merge_Ind_Ocup(
    dt1 = dt_ind_ocup_gen,
    dt2 = dt_ind_ocup_pic,
    dt3 = dt_ind_ocup_otr,
    var.peg = c("CNL_OFICINA", "CNL_JORNADA", "CAJ.AVS", "TERMINAL", "MES.ANO", "LLAVE"),
    var.ide = c("NOMBRE", "REGION", "ZONA", "DEPARTAMENTO", "MUNICIPIO", "GERENCIA.ADMINISTRATIVA")
  )
  
  #elimino bases innecesarias
  rm(dt_ind_ocup_gen, dt_ind_ocup_pic, dt_ind_ocup_otr)
  gc()
  
  # guardando el los indices de ocupacion
  Save_Dt(
    dt = dt_tabla,
    directory = month_out_path,
    filename = "/dt_ind_ocp_",
    periodo = mes.ano
  )
  
  
  
  
  # indices de ocupacion acumulados
  # dt: data con los indices de ocupacion por cajero y avs
  # var.agr: variables de agrupacion de la data
  
  dt_tabla_acumulada <- Ind_Ocup_Acum(
    dt = dt_tabla,
    var.agr = c(
      "CNL_OFICINA",
      "CAJ.AVS",
      "CNL_JORNADA",
      "NOMBRE",
      "REGION",
      "ZONA",
      "DEPARTAMENTO",
      "MUNICIPIO",
      "MES.ANO",
      "LLAVE",
      "GERENCIA.ADMINISTRATIVA"
    )
  )
  
  
  # CALCULANDO LOS CAJEROS PROMEDIO DE CADA OFICINA
  # calculando el numero de cajeros para cada oficina
  
  # dt_caj_nor <-
  #   N_Caj(
  #     dt = dt_master,
  #     var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "DIA"),
  #     var.name = "No.CAJ"
  #   )
  # 
  # # ahora calculo el numero de cajeros promedio  por mes
  # dt_caj_nor <- dt_caj_nor[, .(No.CAJ = round(mean(No.CAJ), 2)),
  #                          by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA")]
  
  
  
  # CAJEROS DE CADA OFICINA
  var1 <-
    c("CAJ.TOT",
      "CAJ.NOR",
      "CAJ.ADI",
      "CAJ.TOT.ESP",
      "CAJ.NOR.ESP",
      "CAJ.ADI.ESP")
  
  # convirtiendo las variables a numericas
  dt_cajeros[, (var1) := lapply(.SD, as.numeric), .SDcol = var1]
  dt_caj_nor <-
    melt(dt_cajeros,
         id.vars = "CNL_OFICINA",
         measure.vars = c("CAJ.NOR", "CAJ.ADI"))
  # cambiando los nombres
  setnames(dt_caj_nor, c("variable", "value"), c("CNL_JORNADA", "No.CAJ"))
  dt_caj_nor[, ':='(CAJ.AVS = "caja", 
                    CNL_JORNADA = ifelse(CNL_JORNADA == "CAJ.NOR",
                                         "J.NOR",
                                         "J.EXT"))]
  # eliminando los ceros
  dt_caj_nor <- dt_caj_nor[No.CAJ != 0]
  
  
  
  dt_prov <- dt_master[MAX.CAJ.AVS == 1] # horas pico
  # obtengo el número de tx por terminal
  dt_prov <-
    dt_prov[, .N, .(CNL_OFICINA, CNL_JORNADA, CAJ.AVS, DIA, HORARIO, TERMINAL)]
  # elimino los cajero que solo trabajaron 15 minutos en esa hora, es decir
  # los que tienen 9 tx o menos
  dt_prov <-  dt_prov[N >= 9]
  
  dt_caj_h_pico <-
    N_Caj(
      dt = dt_prov,
      var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "DIA"),
      var.name = "No.CAJ.H.PICO"
    )
  
  # ahora calculo el numero de cajeros promedio  por mes
  dt_caj_h_pico <-
    dt_caj_h_pico[, .(No.CAJ.H.PICO = round(mean(No.CAJ.H.PICO), 2)),
                  by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA")]
  
  
  
  dt_prov <- dt_master[MAX.CAJ.AVS %in% c(1, 2, 3)] # 3 horas pico
  # obtengo el número de tx por terminal
  dt_prov <-
    dt_prov[, .N, .(CNL_OFICINA, CNL_JORNADA, CAJ.AVS, DIA, HORARIO, TERMINAL)]
  # elimino los cajero que solo trabajaron 15 minutos en esa hora, es decir
  # los que tienen 9 tx o menos
  dt_prov <-  dt_prov[N >= 9]
  
  
  dt_caj_3h_pico <-
    N_Caj(
      dt = dt_prov,
      var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA", "DIA"),
      var.name = "No.CAJ.3H.PICO"
      )
  
  # ahora calculo el numero de cajeros promedio  por mes
  dt_caj_3h_pico <-
    dt_caj_3h_pico[, .(No.CAJ.3H.PICO = round(mean(No.CAJ.3H.PICO), 2)),
                  by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA")]
  
  
  rm(dt_prov)
  
  
  # CALCULANDO LOS CAJEROS MAXIMOS DE CADA OFICINA
  
  dt_caj_nor_max <-
    N_Caj(
      dt = dt_master,
      var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
      var.name = "No.CAJ.MAX"
    )
  
  dt_prov <- dt_master[MAX.CAJ.AVS == 1] # hora pico
  dt_caj_h_pico_max <-
    N_Caj(
      dt = dt_prov,
      var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
      var.name = "No.CAJ.H.PICO.MAX"
    )
  
  dt_prov <- dt_master[MAX.CAJ.AVS %in% c(1, 2, 3)] # 3 horas pico
  dt_caj_3h_pico_max <-
    N_Caj(
      dt = dt_prov,
      var.agr = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
      var.name = "No.CAJ.3H.PICO.MAX"
    )
  
  
  # pegando el numero de cajeros de cada conjunto de datos
  # dt1: data con el numero de cajeros general
  # dt2: data con el numero de cajeros en hora pico
  # dt3: data con el numero de cajeros en las 3 horas pico
  # var.peg: variables para realizar los pegues de la data
  
  dt_caj <- Merge_No_Caj(
    dt1 = dt_caj_h_pico,
    dt2 = dt_caj_3h_pico,
    dt3 = dt_caj_nor,
    var.peg = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
    var.caj = c("No.CAJ", "No.CAJ.H.PICO", "No.CAJ.3H.PICO")
  )
  
  
  # pegando el numero de cajeros maximos de cada conjunto de datos
  # dt1: data con el numero de cajeros general
  # dt2: data con el numero de cajeros en hora pico
  # dt3: data con el numero de cajeros en las 3 horas pico
  # var.peg: variables para realizar los pegues de la data
  
  dt_caj_max <- Merge_No_Caj(
    dt1 = dt_caj_nor_max,
    dt2 = dt_caj_h_pico_max,
    dt3 = dt_caj_3h_pico_max,
    var.peg = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
    var.caj = c("No.CAJ.MAX", "No.CAJ.H.PICO.MAX", "No.CAJ.3H.PICO.MAX")
  )
  
  # eliminando data innecesaria
  rm(
    dt_caj_nor,
    dt_caj_h_pico,
    dt_caj_3h_pico,
    dt_caj_nor_max,
    dt_caj_h_pico_max,
    dt_caj_3h_pico_max
  )
  gc()
  
  # agregando el numero de cajeros promedio a la tabla acumulada
  
  dt_tabla_acumulada <- merge(
    dt_tabla_acumulada,
    dt_caj,
    by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
    all.x = T
  )
  
  # agregando el numero de cajeros maximos a la tabla acumulada
  
  dt_tabla_acumulada <- merge(
    dt_tabla_acumulada,
    dt_caj_max,
    by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
    all.x = T
  )
  
  # dt: tabla con las tasas de ocupacion acumuladas por oficina
  # var.null: variables que no son necesarias en la data
  # var.cast: variable que se quiere cambiar de categoria de fila a columna independiente
  # var.agr: variables de agrupacion
  # var.comb: variables que se quiere transponer (var.cast)
  
  dt_pronostico <-
    Porc_Pronostico(
      dt = dt_tabla_acumulada,
      var.null = c(
        "No_DIAS",
        "CAJ.AVS",
        "PORC.OCUP",
        "SUM.PORC.OCUP",
        "PORC.OCUP.H.PICO",
        "SUM.PORC.OCUP.H.PICO",
        "PORC.OCUP.3H.PICO",
        "SUM.PORC.OCUP.3H.PICO"
      ),
      var.cast = "CNL_JORNADA",
      var.agr = c(
        "CNL_OFICINA",
        "NOMBRE",
        "REGION",
        "ZONA",
        "DEPARTAMENTO",
        "MUNICIPIO",
        "MES.ANO",
        "LLAVE"
      ),
      var.comb = c(
        "MAYORES_1",
        "MENORES_1",
        "OTROS",
        "No_Tx",
        "No.CAJ",
        "MAYORES_1.H.PICO",
        "MENORES_1.H.PICO",
        "OTROS.H.PICO",
        "No_Tx.H.PICO",
        "No.CAJ.H.PICO",
        "MAYORES_1.3H.PICO",
        "MENORES_1.3H.PICO",
        "OTROS.3H.PICO",
        "No_Tx.3H.PICO",
        "No.CAJ.3H.PICO"
      )
    )
  
  
  Save_Dt(
    dt = dt_pronostico,
    directory = month_out_path,
    filename = "/dt_pronostico_",
    periodo = mes.ano
  )
  
  
  # indicadores de ocupacion metodologia 2
  
  dt_ind_met2 <- Ind_Ocup_Acum_Metodo2(
    dt = dt_tabla,
    dt.para = dt_parametros,
    dt.porc.pron = dt_pronostico,
    var.int = c("LLAVE",
                "MAYORES_1.PORC",
                "MENORES_1.PORC",
                "OTROS.PORC",
                "MAYORES_1.H.PICO.PORC",
                "MENORES_1.H.PICO.PORC",
                "OTROS.H.PICO.PORC",
                "MAYORES_1.3H.PICO.PORC",
                "MENORES_1.3H.PICO.PORC",
                "OTROS.3H.PICO.PORC",
                "No.CAJ",
                "No.CAJ.H.PICO",
                "No.CAJ.3H.PICO"),
    var.names = c("PORC.OCUP2",
                  "PORC.OCUP.H.PICO2",
                  "PORC.OCUP.3H.PICO2")
  )
  
  
  # pegando los indices de la nueva metodologia a la tabla acumulada
  dt_tabla_acumulada <- merge(dt_tabla_acumulada,
                              dt_ind_met2,
                              by = c("CNL_OFICINA", "CNL_JORNADA", "CAJ.AVS"),
                              all.x = T)
  
  
  # calculando el numero de oficinas que tienen su porcentaje
  # de ocupacion entre 70 y 90 por jornada y departamento
  dt_ind_departamento <-
    Count_Porc_Ocupacion(dt = dt_tabla_acumulada,
                         var.1 =
                           c("CNL_JORNADA", "DEPARTAMENTO", "MES.ANO"))
  # calculando el numero de oficinas que tienen su porcentaje
  # de ocupacion entre 70 y 90 por jornada y zona
  dt_ind_zona <- Count_Porc_Ocupacion(dt = dt_tabla_acumulada,
                                      var.1 =
                                        c("CNL_JORNADA", "ZONA", "MES.ANO"))
  
  # calculando el numero de oficinas que tienen su porcentaje
  # de ocupacion entre 70 y 90 por jornada de todo el banco
  dt_ind_jornada <- Count_Porc_Ocupacion(dt = dt_tabla_acumulada,
                                      var.1 =
                                        c("CNL_JORNADA", "MES.ANO"))
  
  ##############################
  
  # calculando el numero de oficinas que tienen su porcentaje
  # de ocupacion entre 70 y 90 por jornada de todo el banco
  dt_ind_gerencia <- Count_Porc_Ocupacion(dt = dt_tabla_acumulada,
                                         var.1 =
                                           c("CNL_JORNADA", "GERENCIA.ADMINISTRATIVA", "MES.ANO"))
  
  # guardando 
  Save_Dt(
    dt = dt_ind_gerencia,
    directory = month_out_path,
    filename = "/dt_ind_gerencia_",
    periodo = mes.ano
  )
  
  ################################
  
  
  # guardando 
  Save_Dt(
    dt = dt_ind_departamento,
    directory = month_out_path,
    filename = "/dt_ind_departamento_",
    periodo = mes.ano
  )
  
  # guardando 
  Save_Dt(
    dt = dt_ind_zona,
    directory = month_out_path,
    filename = "/dt_ind_zona_",
    periodo = mes.ano
  )
  
  # guardando 
  Save_Dt(
    dt = dt_ind_jornada,
    directory = month_out_path,
    filename = "/dt_ind_jornada_",
    periodo = mes.ano
  )
  
  # eliminando los indices de ocupacion con la primera metodología
  # y la suma acumulada de los porcentajes de ocupacion
  var.eliminate <- c("PORC.OCUP", "SUM.PORC.OCUP",
                     "PORC.OCUP.H.PICO", "SUM.PORC.OCUP.H.PICO",
                     "PORC.OCUP.3H.PICO", "SUM.PORC.OCUP.3H.PICO")
  dt_tabla_acumulada[, (var.eliminate) := NULL]
  
  # guardando el los indices de ocupacion de la tabla acumulada
  Save_Dt(
    dt = dt_tabla_acumulada,
    directory = month_out_path,
    filename = "/dt_tabla_acumulada_",
    periodo = mes.ano
  )
  
  # agregando los indices de ocupacion a la data de frecuencias
  
  dt_frecu_cash <- merge(
    dt_frecu_cash,
    dt_tabla_acumulada[, .(CNL_OFICINA,
                           CAJ.AVS,
                           CNL_JORNADA ,
                           PORC.OCUP2,
                           PORC.OCUP.H.PICO2,
                           PORC.OCUP.3H.PICO2)],
    by = c("CNL_OFICINA", "CAJ.AVS", "CNL_JORNADA"),
    all.x = T
  )
  
  # guardando la data
  
  Save_Dt(
    dt = dt_frecu_cash,
    directory = month_out_path,
    filename = "/dt_frecu_cash_",
    periodo = mes.ano
  )
}





























































