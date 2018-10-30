# Sets the locale depending on the system that executes the code
if(Sys.info()["sysname"] == "Linux"){
  Sys.setlocale(category = "LC_TIME", locale = "en_US.utf-8")
} else if(Sys.info()["sysname"] == "Windows"){
  Sys.setlocale(category = "LC_ALL", locale = "English")
} else{
  stop(paste("Configure locales for system with name:", Sys.info()["sysname"]))
}


#' Function which is intended for printing strings in the R console using the C syntax
pprint <- function(...){cat(sprintf(...), "\n")}


#' Canonizes a path given a kind of slash.
#' @param path: path to be canonized (character)
#' @param slash: slash symbol to be used to build the path (character; "/" by default for
#' assuring multi-plataform compatibilities) (character)
#' @return: the path canonized (character)
normalize_path = function(path, slash="/"){
  path = sub(pattern = "\\/\\/", replacement = slash, path)
  path = sub(pattern = "\\/", replacement = slash, path)
  path = sub(pattern = "\\\\", replacement = slash, path)
  return(path)
}

#' Builds a path from chunks
#' @params ...: All the chunks of paths to be loaded. 
#' @return: the path joined and normalized (character)
os.path.join <- function(...){
  normalize_path(file.path(...), slash = "/")
}
#' Loads in the global environment all the paths parametrized in the settings.json file
#' @return: None (void)
load_paths_historical <- function(){
  paths <- fromJSON("settings.json")$path
  hist_data_path    <<- os.path.join(paths$data_path, paths$division[1])
  dictionary_path_hist       <<- os.path.join(hist_data_path, paths$data_cat[1])
  original_path_hist       <<- os.path.join(hist_data_path, paths$data_cat[2])
  staging_path_hist       <<- os.path.join(hist_data_path, paths$data_cat[3])
  results_path   <<- paths$results_path
  out_path <<- os.path.join(results_path, paths$result_cat[1])
  month_out_path <<- os.path.join(results_path, paths$result_cat[2])
  scripts_path_hist   <<- os.path.join(paths$scripts_path, paths$division[1])
}

#' Loads in the global environment all the paths parametrized in the settings.json file
#' @return: None (void)
load_paths_forecast <- function(){
  paths <- fromJSON("settings.json")$path
  data_path    <<- os.path.join(paths$data_path, paths$division[2])
  data_cat <- paths$data_cat
  original_path       <<- os.path.join(data_path, data_cat[2])
  staging_path   <<- os.path.join(data_path, data_cat[3])
  meta_path <<- os.path.join(data_path, data_cat[4])
  dictionary_path <<- os.path.join(data_path, data_cat[1])
  results_path   <<- paths$results_path
  forecast_path <<- os.path.join(results_path, paths$division[2])
  scripts_path   <<- os.path.join(paths$scripts_path, paths$division[2])
}

#' Loads in the global environment all the modeling parameters in the settings.json file
#' @return: None (void)
load_historical_parameters <- function(){
  hist_par <- fromJSON("settings.json")$historical
  months_cal <<- hist_par$months_to_calculate
  offices <<- os.path.join(hist_par$oficces_to_calculate)
  months_compile <<- hist_par$months_to_compile
}

#' Loads in the global environment all the modeling parameters in the settings.json file
#' @return: None (void)
load_forecasting_parameters <- function(){
  forecasting <- fromJSON("settings.json")$forecasting
  current_month    <<- forecasting$current_month 
  train_cut    <<- forecasting$train_cut
  test_cut    <<- forecasting$test_cut
  last_test_date    <<- forecasting$last_test_date
  last_forecast_date    <<- forecasting$last_forecast_date
  do.test    <<- forecasting$do.test
  xreg_vector    <<- forecasting$xreg_vector
}

#' Loads in the global environment all the filtering parameters in the settings.json file
#' @return: None (void)
load_filtering_parameters <- function(){
  filtering <- fromJSON("settings.json")$filtering
  current_offices_date    <<- filtering$current_offices_date
  hist_offices_date      <<- filtering$hist_offices_date
}

#' Loads in the global environment all the filtering parameters in the settings.json file
#' @return: None (void)
load_results_parameters <- function(){
  paths <- fromJSON("settings.json")$paths
  results_path   <<- paths$results_path
  hist_path <<- os.path.join(results_path, paths$result_cat[2])
  results <- fromJSON("settings.json")$results
  make.plots    <<- results$make.plots
  plots_path    <<- os.path.join(results_path, "Plots")
  plotsHourly_path    <<- os.path.join(plots_path, "Hourly")
  plotsDaily_path    <<- os.path.join(plots_path, "Daily")
  plotsMonthly_path    <<- os.path.join(plots_path, "Monthly")
  plotsHist_path    <<- os.path.join(plots_path, "Hist")
  tables_path    <<- os.path.join(results_path, "Tables")
  
}
#' Loads in the global environment the libraries. If their are not installe,
#' it installs them and after that it imports them.
#' @return: None (void)
load_common_libraries <- function(){
  import("jsonlite")
  import("data.table") # data handle
  import("forecast") # forecasting methods
  import("lubridate") # date managing 
  import("imputeTS") # kalmans filters to NAs time series
  import("tsoutliers")  # identify time series outliers
  import("randomForest") # randome forest model 
  import("Metrics") # forecast metrics
  import("tcltk") # progress bar 
  import("tictoc") # 
  import("tm")
  import("readxl")
  import("xts")
  import("ggplot2") # visualizations
  import("ggforce") # visualization tools
  import("plotly") # interactive visualizations
  import("grid") # visualizations
  import("gridExtra")
  import("stringi") # paquete para manipular strings
  import("stringr") # paquete para manipular strings
  import("rlist") # manipulacion de listas
  import("pacman")
} 

#' Checks if a library is currently installed, installs it if not, and imports it.
#' @return: None (void)
import <- function(...){
  gb = lapply(..., function(x){
    if(!x %in% installed.packages()){
      pprint("Library '%s' not found. Installing...", x)
      install.packages(x)
    }
    library(x, character.only = TRUE)
    pprint("Library '%s' imported", x)
  })
}

#' Load data, set other parameters and create folders 
loadDataParameters <- function(){
  
  ### Dates parameters 
  train_cut <<-  as.Date(train_cut) # last day of training data
  test_cut <<- as.Date(test_cut) # last day on validation
  last_test_date <<- as.Date(last_test_date) # las day on testing or to forecast recursively
  last_forecast_date <<- as.Date(last_forecast_date)
  
  do.test <<- as.logical(do.test)
  make.plots <<- as.logical(make.plots)
  # Results forecast folder 
  forecastFolder <<- os.path.join(forecast_path,
                                  paste0("Forecast_", current_month))
  dir.create(forecastFolder)
  # results tables folder  
  tablesFolder <<- os.path.join(tables_path, paste0("tables_", current_month))
  dir.create(tablesFolder)
  # results aux folder  
  auxFolder <<- os.path.join(tables_path, paste0("aux_", current_month))
  dir.create(auxFolder)
}

## function to execute proccess 
demandaTransaccional <- function(){
  # Runing historical, forecasting and results process
  ## calculate historical outputs of the model
  pprint("calculating historical outputs of the model")
  calculate_per_month()
  ## compile historical outputs of the model
  pprint("compiling historical outputs of the model")
  calculate_compile()
  ## changing the name of the variables
  rename()
  ## forecasting
  gc()
  pprint("Forecasting offices")
  forecast_qcajeros()
  ## results
  pprint("Processing forecasts")
  results_maker()
}

#' Sets the environment of the by importing the necessary modules, loading the 
#' necessary libraries and loading the parametrized paths
#' @return: None (void)
set_environment <- function(){
  load_common_libraries()
  load_paths_historical()
  load_paths_forecast()
  load_historical_parameters()
  load_forecasting_parameters()
  load_filtering_parameters()
  load_results_parameters()
  source(os.path.join(scripts_path, "text_tools.R"))
  source(os.path.join(scripts_path, "file_tools.R"))
  source(os.path.join(scripts_path, "forecast_qcajeros.R"))
  # historical function 
  source(os.path.join(scripts_path_hist, "Tools.R"))
  source(os.path.join(scripts_path_hist, "m_cajeros2.R"))
  source(os.path.join(scripts_path_hist, "calculate.R"))
  # data transformation functions
  dataTransformation <- "Datatransformation"
  import_module(os.path.join(dataTransformation, "staging_maker.R"))
  import_module(os.path.join(dataTransformation, "master_maker.R"))
  import_module(os.path.join(dataTransformation, "master_maker_recursive.R"))
  # model functions
  models <- "Models"
  import_module(os.path.join(models, "ts_models_train.R"))
  import_module(os.path.join(models, "recursive_forecast.R"))
  import_module(os.path.join(models, "randomForest_train.R"))
  import_module(os.path.join(models, "model_combination.R"))
  import_module(os.path.join(models, "Q_cajeros.R"))
  # utiles functions
  utiles <- "utiles"
  import_module(os.path.join(utiles, "dates_maker.R"))
  import_module(os.path.join(utiles, "text_cleaner.R"))
  import_module(os.path.join(utiles, "theme_ts.R"))
  # result function
  models <- "Results"
  import_module(os.path.join(models, "resultsFunctions.R"))
  import_module(os.path.join(models, "results_maker.R"))
  import_module(os.path.join(models, "plotMaker.R"))
  
  loadDataParameters()
  # Load configuration file and create log
  config <- fromJSON("settings.json")
  jsontest = toJSON(config, pretty = TRUE, auto_unbox = TRUE)
  write(jsontest, file = os.path.join(config$paths$log_path, 
                                     paste0("log", Sys.Date(), ".json")))
  
}


