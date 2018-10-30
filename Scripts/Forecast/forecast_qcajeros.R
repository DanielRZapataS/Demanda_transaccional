
## Forecasting main ##

forecast_qcajeros <- function(){

  
  #Staging
  staging <- staging_maker()
  offices <- unique(staging$LLAVE)
  total <- length(offices) 
  
  #Meta
  holidays <- get.path(meta_path, "holiday") %>% fread()
  holidays[ , FECHA := as.Date(FECHA)]

  Results <- list()
  saver <- c(seq(0, length(offices), 50), length(offices))
  
  set.seed(1) 
  
  # create progress bar
  pb <- tkProgressBar(title = "progress bar", min = 0,
                      max = total, width = 300)
  # Start the clock!
  tic("Q_Cajeros")
  
  #running for each office nature
  for(j in 1:total){
    Results[[j]] <-
      
      Q_cajeros(j, do.test, staging,train_cut,
                test_cut,last_test_date, last_forecast_date,
                holidays, xreg_vector, offices)
    
    setTkProgressBar(pb, j, label=paste( round(j/total*100, 2),
                                         "% done"))
    gc()
    if(j %in% saver|j ==1 |j == 5 | j == 10){
      save(Results, 
           file = os.path.join(forecastFolder,paste0(j,"offices.RDta")))
      Results <- list() }
  }
  
  toc()
}

