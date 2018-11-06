## Daily folder 

## ploting txs time series function for daily data 
plotsTxsDaily <- function(officesDictionary, 
                          staging, 
                          dates_val,
                          dates_test, 
                          dates_forecast,
                          holidays, 
                          total,
                          matrix_results,
                          offices,
                          forecast_daily){
  plotsFolder <- os.path.join(plotsDaily_path, "TXS")
  plotsFolder <- os.path.join(plotsFolder,
                              paste0("Plots_", current_month))
  dir.create(plotsFolder)
  
  ## get staging data
  office_names <- officesDictionary[LLAVE %in% offices, 
                                    .( LLAVE = LLAVE,
                                       name =  paste(NOMBRE, LLAVE),
                                       saver = paste0(LLAVE, text_cleaner(NOMBRE)))]
  ##staging preparation 
  staging[, FECHA := as.Date(FECHA)]
  staging[FECHA < range(dates_val)[1] ,Type := "Training"]
  staging[FECHA %in% dates_val, Type := "Validation"]
  
  ##recursive forecast
  # forecast_daily <- os.path.join(auxFolder, "txsForecastDaily.csv") %>%
  #   fread()
  # forecast_daily[, FECHA := as.Date(FECHA)]
  forecast_daily <- forecast_daily[, .(LLAVE, FECHA, TXS)]
  forecast_daily[, Type := "Recursive forecast"]
  
  # merging
  if(do.test == TRUE){
    staging[FECHA %in% dates_test, Type := "Test"]
    ## test 
    forecast_dailytest <- os.path.join(auxFolder, "txsForecastDailyTest.csv") %>%
      fread()
    forecast_dailytest[, FECHA := as.Date(FECHA)]
    forecast_dailytest <- forecast_dailytest[, .(LLAVE, FECHA, TXS)]
    forecast_dailytest[, Type := "Test forecast"]
    dailyTxsPlot <- rbindlist(list(staging, forecast_dailytest, forecast_daily))
  }else{ 
    dailyTxsPlot <- rbindlist(list(staging,  forecast_daily))
  }
  dailyTxsPlot <- dailyTxsPlot[order(LLAVE, FECHA, Type)]
  dailyTxsPlot <- merge(dailyTxsPlot, holidays, by = "FECHA")
  dailyTxsPlot <- dailyTxsPlot[HOLIDAYS == 0, ]
  dailyTxsPlot[, HOLIDAYS := NULL]
  # making test plot 
  ppi <- 300
  matrix_list <- list()
  j = 1
  if(do.test == TRUE){  
    for(j in 1:total){
    office <- offices[j]
    office_name <- office_names[LLAVE == office, name]
    saver <- office_names[LLAVE == office, saver]
    dailyTxsPlotOff <- na.omit(dailyTxsPlot[LLAVE == office,])
    matrix_result <- matrix_results[LLAVE == office,.(model_name, 
                                                      rmse_train, rmse_val, 
                                                      rmse_test)]
    
    test <- dailyTxsPlotOff[Type == "Test", TXS]
    forecastTest <- dailyTxsPlotOff[Type == "Test forecast", TXS]
    
    error <- test - forecastTest   
    matrix_result <- copy(matrix_result)
    matrix_result[, "MAE test" :=  mae(test, forecastTest)]
    matrix_result[, "MAPE test" := mape(test, forecastTest)]
    matrix_result[, "sMAPE test" := smape(test, forecastTest)]
    matrix_result[, "Mean TRX" := mean( test)]
    matrix_result[, "Max under-forcst" := max(error)]
    matrix_result[, "Max over-forcst" := min(error)]
    matrix_result[, "Under-forcst" := sum(error > 0)]
    matrix_result[, "over-forecst" := sum(error < 0)]
    matrix_result[, "MAETest/meanTRX" := (`MAE test`/`Mean TRX`)*100]
    matrix_list[[j]] <- matrix_result
    
    # for plot 
    matrix <- copy(matrix_result)
    matrix[, model_name := NULL]
    matrix <- matrix[,  lapply(.SD, round, digits = 2)]
    tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
    matrix <- tableGrob(matrix, rows=NULL, theme=tt)
    
    
    plot <-
      ggplot(dailyTxsPlotOff, aes(FECHA, TXS, color = Type, linetype = Type)) +
      geom_line(size = 0.8, alpha = 0.7) +
      ylab("Transactions") +
      xlab("Date") +
      labs(
        title = paste0("Forecast ",
                       office_name, " training, validation, and test"),
        subtitle = matrix_result$model_name
      ) +
      geom_vline(
        xintercept = as.numeric(c(train_cut, test_cut, last_test_date)),
        linetype = 4,
        size = 1.2,
        color = "darkgreen"
      ) +
      facet_zoom(x = FECHA %in% c(dates_test, dates_forecast),
                 zoom.size = 1.2)  +
      scale_linetype_manual(values = c(1, 1, 2, 1, 1)) +
      theme_ts + theme(legend.position = "top")
    
    png(
      paste0(os.path.join(plotsFolder, saver), ".png"),
      width = 15 * ppi,
      height = 10 * ppi,
      res = ppi
    )
    print(grid.arrange(
      plot,
      matrix,
      nrow = 2,
      as.table = TRUE,
      heights = c(3, 1)
    ))
    dev.off()
    
    }
    
  }
  if(do.test == FALSE){  
    for(j in 1:total){
      office <- offices[j]
      office_name <- office_names[LLAVE == office, name]
      saver <- office_names[LLAVE == office, saver]
      dailyTxsPlotOff <- na.omit(dailyTxsPlot[LLAVE == office,])
      matrix_result <- matrix_results[LLAVE == office,.(model_name, 
                                                        rmse_train, rmse_val)]
      
      plot <-
        ggplot(dailyTxsPlotOff, aes(FECHA, TXS, color = Type, linetype = Type)) +
        geom_line(size = 0.8, alpha = 0.7) +
        ylab("Transactions") +
        xlab("Date") +
        labs(
          title = paste0("Forecast ",
                         office_name, " training and validation"),
          subtitle = matrix_result$model_name
        ) +
        geom_vline(
          xintercept = as.numeric(c(train_cut, test_cut)),
          linetype = 4,
          size = 1.2,
          color = "darkgreen"
        )  +
        scale_linetype_manual(values = c(2, 1, 1)) +
        theme_ts + theme(legend.position = "top")
      
      png(
        paste0(os.path.join(plotsFolder, saver), ".png"),
        width = 15 * ppi,
        height = 10 * ppi,
        res = ppi
      )
      print(plot)
      dev.off()
      
    }
  }
  
}

## ploting capacty utilization rate by hour 
plotsHourly <- function(forecast_hourly, officesDictionary, offices, mes = NULL ){
  plotsFolder <- os.path.join(plotsHourly_path, "TO")
  plotsFolder <- os.path.join(plotsFolder,
                              paste0("Plots_", current_month))
  dir.create(plotsFolder)
  
  office_names <- officesDictionary[LLAVE %in% offices, 
                                    .( LLAVE = LLAVE,
                                       name =  paste(NOMBRE, LLAVE),
                                       saver = paste0(LLAVE, text_cleaner(NOMBRE)))]
 
  offices <- unique(forecast_hourly$LLAVE)
  if(is.null(mes)){mes <- unique(forecast_hourly$PERIODO)}
  ppi = 300
  for(i in 1:length(offices)){
    for(j in 1:length(mes)){
      office_name <- office_names[LLAVE == offices[i], name]
      saver <- office_names[LLAVE == offices[1], saver]
      dataplot <- forecast_hourly[LLAVE == offices[i] & PERIODO == mes[j]]
      fhorario <- unique(dataplot$HORARIO)
      dataplot[, HORARIO := factor(HORARIO, levels = fhorario,
                                   ordered = T)]
      
      tasaOcu <- dataplot[LLAVE == offices[i] & PERIODO == mes[j],
                          .(HORARIO, TASA.OCU.TEO, TASA.OCU.OBS)]
      tasaOcu <- melt(tasaOcu, id.vars = c("HORARIO"))
      
      cajeros <- dataplot[LLAVE == offices[i] & PERIODO == mes[j],
                          .(HORARIO, CAJ_TOTAL, CAJ_HORA)]
      cajeros <- melt(cajeros, id.vars = c("HORARIO"))
      cajeros[, value := as.character(value)]
      cajeros[variable != "CAJ_HORA", value := "" ]
      
      horaPico <- dataplot[LLAVE == offices[i] & PERIODO == mes[j] ,
                           .(HORARIO,TRX_RATE, TASA.OCU.TEO, TASA.OCU.OBS)]
      
      horaind = max(horaPico$TRX_RATE)
      horaPicoPlot <- which(horaPico[TRX_RATE == horaind,
                                     HORARIO][1] == levels(horaPico$HORARIO))
      
      trx_rateMax <- horaPico[ ,max(TASA.OCU.TEO, TASA.OCU.OBS)]
      
      ## tabla 
      tabla <-
        dataplot[1, .(
          PERIODO = PERIODO,
          Oficina = LLAVE,
          Cajeros = CAJ_TOTAL,
          "Tasa de servicio" = SERVICE_RATE,
          "TRX hora pico" = round(horaind, 1)
        )]
      
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      table2 <- tableGrob(tabla, rows=NULL, theme=tt)
      
      ## grafica 
      p1 <-
        ggplot(tasaOcu, aes(x = HORARIO, y = value, fill = variable)) +
        geom_bar(stat = "identity",
                 width = 0.5,
                 position = position_dodge(0.7)) +
        geom_text(
          aes(
            label = cajeros$value,
            x = cajeros$HORARIO,
            y = tasaOcu$value
          ),
          position = position_dodge(width = 0.8),
          vjust = -0.6
        ) +
        geom_hline(yintercept = 1,
                   color = 'red',
                   linetype = "dashed") +
        geom_hline(yintercept = 0.7,
                   color = 'blue',
                   linetype = "dashed") +
        geom_hline(yintercept = 0.9,
                   color = 'blue',
                   linetype = "dashed") +
        xlab("Horarios") + ylab("Capacity utilization rate") +
        scale_fill_manual(
          values = c("#3B9AB2", "#E1AF00"),
          name = "Capacity utilization rate ",
          breaks = c(
            "TASA.OCU.TEO",
            "TASA.OCU.OBS"
          ),
          labels = c(
            "Theoretical",
            "Observed"
          )
        ) +
        annotate(
          "rect",
          xmin = horaPicoPlot - 0.4,
          xmax = horaPicoPlot + 0.4,
          ymin = 0,
          ymax = trx_rateMax,
          alpha = .2,
          color = "chartreuse3"
        ) +
        labs(
          title = paste0("Capacity utilization rate office ", tolower(office_name))
        )
      
      #Exportar datos
      
      png(
        paste0(os.path.join(plotsFolder, offices[i]),
               "_", mes[j], ".png"),
        width = 12 * ppi,
        height = 7 * ppi,
        res = ppi
      )
      print(grid.arrange(
        p1,
        table2,
        nrow = 2,
        as.table = TRUE,
        heights = c(3, 1)
      ))
      dev.off()    
    }
  }
}
## metrics overal on the model 
hist_maker <- function(){
  metricsForecasts <-
    os.path.join(auxFolder, "forecastMetrics.csv") %>% fread()
  plotsFolder <- os.path.join(plotsHist_path,
                              paste0("Plots_", current_month))
  dir.create(plotsFolder)
  
  
  ppi <- 300
  plot1 <- ggplot(metricsForecasts, aes(x = `MAPE test`)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") +
    theme_minimal() +
    xlab("MAPE") +  ylab("Density") +
    geom_vline(data = metricsForecasts, aes(xintercept = mean(`MAPE test`)),
               linetype = "dashed") +
    labs(
      title = paste(
        "Mean absolute percentage error on test",
        test_cut,
        last_test_date
      ),
      subtitle = paste("N =", nrow(metricsForecasts))
    ) +
    theme(text = element_text(size = 20))
  matrix <- data.frame(unclass(summary(metricsForecasts$`MAPE test`) ), 
                       check.names = FALSE, stringsAsFactors = FALSE)
  matrix <- round(matrix, 4) %>% t %>% data.table()    
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  matrix <- tableGrob(matrix, rows=NULL, theme=tt)
  
  png(
    os.path.join(plotsFolder, "MAPE.png"),
    width = 20 * ppi,
    height = 10 * ppi,
    res = ppi
  )
  print(grid.arrange(
    plot1,
    matrix,
    nrow = 2,
    as.table = TRUE,
    heights = c(3, 1)
  ))
  dev.off()
  
  plot2 <- ggplot(metricsForecasts, aes(x = `sMAPE test`)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") +
    theme_minimal() +
    xlab("MAPE") +  ylab("Density") +
    geom_vline(data = metricsForecasts, aes(xintercept = mean(`sMAPE test`)),
               linetype = "dashed") +
    labs(
      title = paste(
        "Symmetric mean absolute percentage error on test",
        test_cut,
        last_test_date
      ),
      subtitle = paste("N =", nrow(metricsForecasts))
    ) +
    theme(text = element_text(size = 20))
  matrix <- data.frame(unclass(summary(metricsForecasts$`sMAPE test`) ), 
                       check.names = FALSE, stringsAsFactors = FALSE)
  matrix <- round(matrix, 4) %>% t %>% data.table()    
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  matrix <- tableGrob(matrix, rows=NULL, theme=tt)
  
  png(
    os.path.join(plotsFolder, "sMAPE.png"),
    width = 20 * ppi,
    height = 10 * ppi,
    res = ppi
  )
  print(grid.arrange(
    plot2,
    matrix,
    nrow = 2,
    as.table = TRUE,
    heights = c(3, 1)
  ))
  dev.off()
}
