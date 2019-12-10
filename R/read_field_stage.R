#'
#'@title Read in a field stage file
#'
#'@param file Path to stage TSV or CSV file
#'@details
#'will read file from an assumed standard format.
#'@description
#'Reads a field file for stage. Field file must be either
#'a TSV (tab separated) or CSV (comma separated) file with 
#'columns named "datetime" and "stage" (in meters). Datetime
#'format must be yyyy-mm-dd
#'@author Luke Winslow
#'@examples
#'
#'field_file = system.file('extdata/field_stage.csv', 
#'                          package='glmtools')
#'
#'data <- read_field_stage(field_file)
#'
#'plot(data)
#'
#'@export
read_field_stage <- function(file){
	
	delimiter <- get_delimiter(file)
	data <- read.delim2(file = file, header = TRUE, sep = delimiter, stringsAsFactors = FALSE)
	
	date_i <- match('datetime',tolower(names(data)))
	stage_i <- match('stage',tolower(names(data)))
	
	if (any(is.na(c(date_i, stage_i)))){
		stop('format for field data not supported. Columns must be DateTime and Stage')
	}
	
	df <- data.frame("DateTime" = coerce_date(data[, date_i]), 
									 "Stage" = as.numeric(data[, stage_i]))
	
	df <- df[order(df$DateTime), ] #sort validation data by datetime, then depth
	
	return(df)
}

