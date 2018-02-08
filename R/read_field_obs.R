#' Read in field data into a data.frame
#'
#' @param file 
#' a file path for a supported field data format (currently, tsv
#' or csv with Date, depth, wtemp or similar columns)
#' @param var_name 
#' Name of variable to look for in field_obs file. 
#' Should match a GLM simulation variable (see output from \code{\link{sim_vars}}).
#' Defaults to 'temp'
#' @return a data.frame with DateTime, Depth, and wTemp columns
#' @keywords methods
#' @seealso \link{get_temp}, \link{resample_sim}
#' @author
#' Jordan S. Read
#' @examples 
#' # -- read in a .csv file --
#' file <- system.file('extdata', 'field_data.csv', package = 'glmtools')
#' field_data <- read_field_obs(file)
#'
#' # -- read in a .tsv file --
#' file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#' field_data <- read_field_obs(file)
#' @import tools
#' @importFrom utils read.delim2
#' @export
read_field_obs <- function(file, var_name='temp'){
	
  if(file_ext(file) == "rds"){
    data <- readRDS(file)
  } else {
    delimiter <- get_delimiter(file)
    data <- read.delim2(file = file, header = TRUE, sep = delimiter, stringsAsFactors = FALSE)
  }
  
  date_i <- match('datetime', tolower(names(data)))
  depth_i <- match('depth', tolower(names(data)))
  var_i <- match(tolower(var_name), tolower(names(data))) #only use lowercase in find, need case consistent later
  #if (is.na(var_i) && var_name == 'temp'){
  	#leave this in here to catch wtemp header
  #  var_i <- match('wtemp',tolower(names(data)))
  #}
  
  if (any(is.na(c(date_i, depth_i, var_i)))){stop(sprintf('format for field data not supported. Need DateTime, Depth, and %s column names', var_name))}
  

  df <- data.frame("DateTime" = coerce_date(data[, date_i]), 
                   "Depth" = as.numeric(data[, depth_i]), 
                   "Var" = as.numeric(data[, var_i]))
  
  #default to wTemp for backward compatibility, otherwise use supplied var_name
  #if(var_name == "temp" || var_name == 'wtemp'){
  #	names(df)[3] = "wTemp"
  #}else{
  	names(df)[3] = var_name
  #}
  
  df <- df[order(df$DateTime, df$Depth), ] #sort validation data by datetime, then depth
  
  return(df)
}

get_delimiter <- function(file){
  delims <- list('csv'=',','tsv'='\t')
  ext <- file_ext(file)
  if (!is.character(ext)){stop('could not parse delimeter for file')}
  delim <- delims[[ext]]
  return(delim)
  
}