## results forecasting main ##

results_maker <- function() {
  ## get staging data
  staging <- get.path(staging_path, current_month) %>% fread()
  #Meta
  holidays <- get.path(meta_path, "holiday") %>% fread()
  holidays[, FECHA := as.Date(FECHA)]

  offices <- unique(staging$LLAVE)
  total <- length(offices)
  
  ### Validation days
  dates_val <- dates_maker(from = train_cut, to = test_cut)
  dates_val <- dates_val$FECHA
  ## forecast
  dates_forecast <-
    dates_maker(from = last_test_date, to = last_forecast_date)
  dates_forecast <- dates_forecast$FECHA
  if (do.test == TRUE) {
    ## test
    dates_test <- dates_maker(from = test_cut, to = last_test_date)
    dates_test <- dates_test$FECHA
  }
  
  # importing results
  results <- importForecastResults(forecastFolder)
  lengthResults <- sapply(results, "[[", "rmse_train") %>% length()
  
  ## label 
  for(i in 1:length(results)){names(results[[i]])[1] <- "oficina"}
  
  # Model counting
  countingModel <-  countingModels(results)
  write.csv(countingModel,
            os.path.join(auxFolder, "countingModels.csv"))
  
  # rmse values
  matrix_results <- rmseResults(results)
  fwrite(matrix_results, os.path.join(auxFolder, "rmse.csv"))
  
  
  # forecast TXS daily and monthly
  if (do.test == TRUE) {
    forecast_daily <- forecastDaily_base(results, dates_test, holidays,
                                         offices, do.test = TRUE)
    forecast_daily[, LLAVE := as.character(LLAVE)]
    fwrite(forecast_daily,
           os.path.join(auxFolder, "txsForecastDailyTest.csv"))
  }
  
  forecast_daily <-
    forecastDaily_base(results, dates_forecast, holidays,
                       offices, do.test = FALSE)
  forecast_daily[, LLAVE := as.character(LLAVE)]
  fwrite(forecast_daily,
         os.path.join(auxFolder, "txsForecastDaily.csv"))
  forecast_monthly <-
    forecast_daily[, .(TXS = sum(TXS)), by = .(LLAVE, PERIODO)]
  
  
  # counting work days by forecast months
  dias_hbBase <- dias_hb(last_test_date, last_forecast_date, holidays)
  
  # office dictionary
  officesDictionary <- get.path(dictionary_path, "Dictionary") %>%
    fread(colClasses = "character")
  
  # forecast Metrics on test 
  if(do.test){
    forecastMetricsBase <- forecastMetrics(officesDictionary, staging, 
                                           dates_test, holidays, total,
                                           matrix_results, offices)
    fwrite(forecastMetricsBase, 
           os.path.join(auxFolder, "forecastMetrics.csv"))
  }
  
  
  ############ calculate capacity utilization rate
  
  ## get historical results of cash frecuency, transactional mixture and cashiers
  # only take data from current month
  
  #cash frecuency and mean cashiers by hour
  frecuencia <-
    get.path(hist_path, paste0("dt_frecu_cash_", current_month)) %>% fread()
  frecuencia <- frecuencia[CAJ.AVS == "caja"]
  frecuencia <- frecuencia[is.na(No.CAJ.H) != T]
  frecuencia[, PROP_HORA := SUM.Tx / sum(SUM.Tx), by = .(LLAVE, MES)]
  frecuencia <- frecuencia[, .(LLAVE, HORARIO, PROP_HORA,
                               CAJ_HORA = No.CAJ.H)]
  frecuencia[, sum(PROP_HORA), by = LLAVE][, sum(V1)] == uniqueN(frecuencia$LLAVE)
  frecuencia[, LLAVE := as.character(LLAVE)]
  # transaxinal mixture and total cashiers by month
  porcEfectivo <-
    get.path(hist_path, paste0("dt_pronostico_", current_month)) %>% fread()
  numerics <- c("No.CAJ",
                "MAYORES_1.PORC",
                "MENORES_1.PORC",
                "OTROS.PORC")
  porcEfectivo[, (numerics) := lapply(.SD, as.numeric), .SDcols = numerics]
  porcEfectivo <- porcEfectivo[, .(
    LLAVE = as.character(LLAVE),
    MAYORES_1.PORC,
    MENORES_1.PORC,
    OTROS.PORC,
    CAJ_TOTAL = No.CAJ
  )]
  porcEfectivo <-
    porcEfectivo[duplicated(porcEfectivo$LLAVE) == FALSE]
  
  # offices which do not match between forecast and historical results
  comp <-
    merge(forecast_daily[, .(LLAVE = unique(LLAVE), pronostico = unique(LLAVE))],
          frecuencia[, .(LLAVE = unique(LLAVE) , frecucia = unique(LLAVE))] ,
          by = "LLAVE", all = T)
  comp <- comp[is.na(pronostico) | is.na(frecucia)]
  fwrite(comp, os.path.join(auxFolder, "comparacionLlaves.csv"))
  
  # parameters service rate cashiers
  parameters <- get.path(dictionary_path_hist, "parametros") %>% fread()
  
  PESO.MAY <- parameters$PESO.MAY
  PESO.MEN <- parameters$PESO.MEN
  PESO.OTR <- parameters$PESO.OTR
  HORAS.NORM <- parameters$HORAS.NORM
  HORAS.EXT <- parameters$HORAS.EXT
  
  
  # Montly
  forecast_monthly <-
    merge(forecast_monthly, porcEfectivo, by = "LLAVE")
  forecast_monthly <- merge(forecast_monthly, dias_hbBase, by = "PERIODO")
  forecast_monthly[, auxVar := substr(LLAVE, 1, 2) ]
  forecast_monthly[auxVar == 21, TASA.OCU := TXS * (1 / (
    HORAS.NORM * CAJ_TOTAL * DIAS_HB *  (MENORES_1.PORC * PESO.MEN + MAYORES_1.PORC * PESO.MAY +
                                    OTROS.PORC * PESO.OTR)
  ))]
  forecast_monthly[auxVar == 22, TASA.OCU := TXS * (1 / (
  HORAS.EXT * CAJ_TOTAL * DIAS_HB *  (MENORES_1.PORC * PESO.MEN + MAYORES_1.PORC * PESO.MAY +
                                    OTROS.PORC * PESO.OTR)
  ))]
  forecast_monthly[, auxVar := NULL]
  forecast_monthly[CAJ_TOTAL == 0, TASA.OCU := 0]
  
  forecast_monthly <-
    merge(forecast_monthly, officesDictionary, by = "LLAVE")
  forecast_monthly[, LLAVEMES := paste0(LLAVE, PERIODO)]
  fwrite(forecast_monthly,
         os.path.join(tablesFolder, "forecast_monthly.csv"))
  
  # Hourly
  forecast_hourly <- subset(forecast_monthly, select = -TASA.OCU)
  forecast_hourly <-
    merge(forecast_hourly,
          frecuencia,
          by = "LLAVE",
          allow.cartesian = T)
  forecast_hourly[, SERVICE_RATE := MAYORES_1.PORC * PESO.MAY + MENORES_1.PORC *
                    PESO.MEN + OTROS.PORC * PESO.OTR]
  
  forecast_hourly[, TRX_PORC := TXS * PROP_HORA]
  forecast_hourly[, TRX_RATE := TXS * PROP_HORA / DIAS_HB]
  forecast_hourly[, TASA.OCU.TEO := TRX_RATE / (CAJ_TOTAL * SERVICE_RATE)]
  forecast_hourly[CAJ_TOTAL == 0, TASA.OCU.TEO := 0]
  forecast_hourly[, TASA.OCU.OBS := TRX_RATE / (CAJ_HORA * SERVICE_RATE)]
  forecast_hourly[, HORAPICO := ifelse(max(TRX_RATE) == TRX_RATE, 1, 0), 
                  by = .(LLAVE, PERIODO)]
  forecast_hourly[, LLAVEMES := paste0(LLAVE, PERIODO)]
  forecast_hourly[, LLAVEMESHORAPICO := paste0(LLAVE, PERIODO, HORAPICO)]
  forecast_hourly[, LLAVEMESHORA := paste0(LLAVE, PERIODO, HORARIO)]
  names_hourly <- names(forecast_hourly)
  names_hourly <- c(names_hourly[which(names_hourly != "GER_ADMIN")], "GER_ADMIN")
  setcolorder(forecast_hourly, names_hourly)
  fwrite(forecast_hourly,
         os.path.join(tablesFolder, "forecast_hourly.csv"))
  
  metatVariablesMontly <-
    data.table(variables = names(forecast_monthly))
  metatVariablesMontly[, type := sapply(forecast_monthly, class)]
  metatVariablesMontly[, length := sapply(forecast_monthly, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariablesMontly,
    os.path.join(tablesFolder, "metatVariablesForecastMontly.csv")
  )
  
  metatVariablesHourly <-
    data.table(variables = names(forecast_hourly))
  metatVariablesHourly[, type := sapply(forecast_hourly, class)]
  metatVariablesHourly[, length := sapply(forecast_hourly, function(x) {
    x <- as.character(x)
    return(max(nchar(x)))
  })]
  fwrite(
    metatVariablesHourly,
    os.path.join(tablesFolder, "metatVariablesForecastHourly.csv")
  )
  # salida consolidado oficina 
  consolidado <- copy(forecast_monthly)
  newcolorder <- names(consolidado)
  newcolorder <- c("CNL_OFICINA", newcolorder[ newcolorder %!in% "CNL_OFICINA"])
  setcolorder(consolidado, newcolorder)
  fwrite(consolidado,
         os.path.join(tablesFolder, "consolidado.csv"))
  
  if (make.plots) {
    # plotsTxsDaily(officesDictionary,
    #               staging,
    #               dates_val,
    #               dates_test,
    #               dates_forecast,
    #               holidays,
    #               total,
    #               matrix_results,
    #               offices,
    #               forecast_daily)
    plotsHourly(forecast_hourly,
                officesDictionary,
                offices,
                mes = unique(forecast_hourly$PERIODO)[1])
     hist_maker()
  }
}
