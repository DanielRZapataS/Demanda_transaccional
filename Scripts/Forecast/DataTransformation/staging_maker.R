# update staging table
#' @return : staging table
staging_maker <- function(){
  # Upload staging field
  current_monthYM <- zoo::as.yearmon(current_month, "%Y-%m")
  previous_month <- floor_date(as.Date(current_monthYM) - months(1), "month")
  previous_month <- substr(previous_month, 1,7)

  stagingFile_path <- get.path(staging_path, previous_month)
  staging <- fread(stagingFile_path, colClasses = "character")
  
  #Upload last original file 
  originalFile_path <- get.path(original_path, current_month)
  original <- fread(originalFile_path, colClasses = "character")
  
  # data quality
  if(sum(colnames(staging) == colnames(original)) != 3){
    stop("Data, or dictionary has NA observations")
  }
  if(sum(colSums(is.na(staging))) > 0 | sum(colSums(is.na(original))) > 0){
    stop("Staging or raw has NA observations")
  }
  
  staging[, FECHA := gsub("/", "-", FECHA ) ]
  original[, FECHA := gsub("/", "-", FECHA ) ]
  
  staging[, FECHA := as.Date(FECHA, "%Y-%m-%d")]
  original[,FECHA := as.Date(FECHA, "%d-%m-%Y") ]
  
  if(range(staging$FECHA)[2] > range(original$FECHA)[1]){
    stop("Last staging date is greater than first raw data")
  }
  
  # merge two files 
  staging <- rbindlist(list(staging, original) )
  staging <- staging[order(LLAVE, FECHA)]
  
  # filters 
  files_meta <- list.files(meta_path)
  exclude_offices <- grep("exclude", files_meta, value = TRUE)
  exclude_offices <- os.path.join(meta_path, exclude_offices)
  exclude_offices <- fread(exclude_offices)  
  exclude_offices <- exclude_offices$LLAVE %>% unique
  exclude_offices <- c(exclude_offices, "222390", "223250", "226150")
  
  current_offices <- staging[, .(LAST_DATE = range(FECHA)[2]),
                             by = LLAVE][LAST_DATE > current_offices_date, LLAVE]
  hist_offices <- staging[, .(LAST_DATE = range(FECHA)[1]),
                          by = LLAVE][LAST_DATE < hist_offices_date, LLAVE]
  
  staging <- staging[!(LLAVE %in% exclude_offices) ]
  staging <- staging[LLAVE %in% current_offices  & 
                       LLAVE %in% hist_offices]
  staging[, TXS := as.numeric(TXS)]
  
  fwrite(staging,
         os.path.join(staging_path, paste0("staging_" , current_month, ".csv")),
         row.names = F)
  return(staging)
}
