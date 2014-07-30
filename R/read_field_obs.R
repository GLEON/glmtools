#'@title read in field data into a data.frame
#'
#'@param file a file path for a supported field data format (currently, tsv or csv with Date, depth, wtemp or similar columns)
#'@return a data.frame with DateTime, Depth, and wTemp columns
#'@keywords methods
#'@seealso \link{get_temp}, \link{resample_time}
#'@author
#'Jordan S. Read
#'@examples 
#'# -- read in a .csv file --
#'file <- system.file('extdata', 'field_data.csv', package = 'glmtools')
#'field_data <- read_field_obs(file)
#'
#'# -- read in a .tsv file --
#'file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#'field_data <- read_field_obs(file)
#'@import tools
#'@export
read_field_obs <- function(file){
  
  delimiter <- get_delimiter(file)
  data <- read.delim2(file = file, header = TRUE, sep = delimiter)
  
  date_i <- match('datetime',tolower(names(data)))
  depth_i <- match('depth',tolower(names(data)))
  wtr_i <- match('temp',tolower(names(data)))
  if (is.na(wtr_i)){
    wtr_i <- match('wtemp',tolower(names(data)))
  }
  
  if (any(is.na(c(date_i, depth_i, wtr_i)))){stop('format for field data not supported. Need DateTime, Depth, and wTemp column names')}
  
  df <- data.frame("DateTime" = as.POSIXct(data[, date_i]), "Depth" = data[, depth_i], "wTemp" = data[, wtr_i])
  
  return(df)
}

get_delimiter <- function(file){
  delims <- list('csv'=',','tsv'='\t')
  ext <- file_ext(file)
  delim <- delims[[ext]]
  return(delim)
  
}