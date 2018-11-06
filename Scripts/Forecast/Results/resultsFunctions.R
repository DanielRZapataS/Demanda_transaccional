#' Import results from forecasts
#' @param pathResults : path of the forecasts results 
#' @return: list with all the results  
importForecastResults <- function(pathResults){
  results <- list()
  files <- list.files(pathResults)
  position <- sapply(strsplit(files, "o"), "[[", 1) %>% as.numeric
  location <- data.table(files = files , position = position ) 
  location <- location[order(position)] 
  iter1 <- c(0, location$position)
  through <- seq(1,length(iter1)-1)
  
  for(i in through){
    along <- seq(iter1[i]+1, iter1[i+1])
    upload <- paste(pathResults, location[i, files], sep = "/")
    load(upload)
    for(j in along){
      results[[j]] <- Results[[j]]
    }
  }
  return(results)
}

#' Counting model type 
#' @param results: list of results 
#' @return: counting of model types  
countingModels <- function(results){
  # is there any offices withouth forecast ?
  models <- sapply(results, "[[", "model_name")
  if(sum(sapply(models, is.null)) > 0){
    print("There are offices without forecasts")
    models <- models[-which(sapply(models, is.null))]
  }
  
  # Counting type models
  models <- unlist(models)
  models <- text_cleaner(models)
  docs <- Corpus(VectorSource(models))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(freq=v)
  return(d)
}

#' RMSE results 
#' @param results: list of results 
#' @return: dt with rmse values of trainig, validation and test for all offices 
rmseResults <- function(results){
  matrix_results <- data.table(LLAVE = sapply(results, "[[", "oficina"))
#  matrix_results <- data.table(LLAVE = offices)
  matrix_results[, model_name := sapply(results, "[[", "model_name")]
  matrix_results[, rmse_train := sapply(results, "[[", "rmse_train")]
  matrix_results[, rmse_val := sapply(results, "[[", "rmse_val")]
  if(do.test == TRUE){
    matrix_results[, rmse_test := sapply(results, "[[", "rmse_test")]
  }
  return(matrix_results)
}

## test results  
forecastMetrics <- function(officesDictionary, staging, 
                            dates_test, holidays, total,
                            matrix_results, offices){

  ## get staging data
  office_names <- officesDictionary[LLAVE %in% offices, 
                                    .( LLAVE = LLAVE,
                                       name =  paste(NOMBRE, LLAVE),
                                       saver = paste0(LLAVE, text_cleaner(NOMBRE)))]
  ##staging preparation 
  staging[, FECHA := as.Date(FECHA)]
  staging <- staging[FECHA %in% dates_test]
  staging <- staging[, Type := "Test"]
  
  ## test 
  forecast_dailytest <- os.path.join(auxFolder, "txsForecastDailyTest.csv") %>%
    fread()
  forecast_dailytest[, FECHA := as.Date(FECHA)]
  forecast_dailytest <- forecast_dailytest[, .(LLAVE, FECHA, TXS)]
  forecast_dailytest[, Type := "Test forecast"]
  dailyTxsPlot <- rbindlist(list(staging,  forecast_dailytest))
  
  dailyTxsPlot <- dailyTxsPlot[order(LLAVE, FECHA, Type)]
  dailyTxsPlot <- merge(dailyTxsPlot, holidays, by = "FECHA")
  dailyTxsPlot <- dailyTxsPlot[HOLIDAYS == 0, ]
  dailyTxsPlot[, HOLIDAYS := NULL]

 
  matrix_list <- list()
 
    for(j in 1:total){
      office <- offices[j]
      dailyTxsPlotOff <- na.omit(dailyTxsPlot[LLAVE == office,])
      matrix_result <- matrix_results[LLAVE == office]
      
      test <- dailyTxsPlotOff[Type == "Test", TXS]
      forecastTest <- dailyTxsPlotOff[Type == "Test forecast", TXS]
      
      error <- test - forecastTest   
      matrix_result <- copy(matrix_result)
      matrix_result[, "MAE test" :=  mae(test, forecastTest)]
      matrix_result[, "MAPE test" := mape(test, forecastTest)]
      matrix_result[, "sMAPE test" := smape(test, forecastTest)]
      matrix_result[, "Mean TRX" := mean( test)]
      matrix_result[, "Max under-forecast" := max(error)]
      matrix_result[, "Max over-forecast" := min(error)]
      matrix_result[, "Under-forecast" := sum(error > 0)]
      matrix_result[, "over-forecast" := sum(error < 0)]
      matrix_result[, "MAETest/meanTRX" := (`MAE test`/`Mean TRX`)*100]
      matrix_list[[j]] <- matrix_result
      
    }
    matrix_results <- rbindlist(matrix_list)
    return(matrix_results)
  
}


#' Forecast daily dt 
#' @param results: list of results 
#' @param dates: vector of forecasts dates
#' @param holidays: holidays dummy dt 
#' @param offices: vector of offices
#' @return: Forecast daily dt
forecastDaily_base <- function(results, dates, holidays, offices, do.test){
  tabla_results <- list()
  for(j in c(1:length(results))){
    if(do.test){
      TXS <- results[[j]]$forecast_test     
    }else{
      TXS <- results[[j]]$forecast_rec      
    }

    forecast <- data.table(FECHA = dates, TXS)
    forecast[, MES := month(FECHA)]
    forecast[, MES := ifelse(nchar(MES)>1, MES, paste0("0", MES))]
    forecast[, PERIODO := paste(year(FECHA), MES, sep = "-" )]
    forecast[, MES := NULL]
    forecast <- merge(forecast, holidays, by = "FECHA")
    forecast[HOLIDAYS == 1, TXS := 0]
    forecast[, LLAVE := results[[j]]$oficina]
    tabla_results[[j]] <- forecast  
  }
  
  forecast_daily <- rbindlist(tabla_results)
  
  return(forecast_daily)
}

#' Get work days for forecast months
#' @param last_test_date: last test date
#' @param last_forcast_date: last forecast date
#' @param holidays: holidays dummy dt 
#' @return: data.table of work days by month of forecast range
dias_hb <- function(last_test_date, last_forecast_date, holidays){
  dias_hb <- dates_maker(from = last_test_date, to = last_forecast_date) %>% 
    merge(holidays, by = "FECHA") %>% 
    data.table()
  dias_hb[, MES := month(FECHA)]
  dias_hb[, MES := ifelse(nchar(MES)>1, MES, paste0("0", MES))]
  dias_hb[, PERIODO := paste(year(FECHA), MES, sep = "-" )]
  dias_hb[, MES := NULL]
  dias_hb <- dias_hb[HOLIDAYS == 0, ]
  dias_hb <- dias_hb[, .(DIAS_HB=.N), by = PERIODO]
  return(dias_hb)
}
