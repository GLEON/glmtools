
#' @importFrom ncdf4 nc_open
get_glm_nc  <-  function(file){
	if(length(file) < 1 || is.na(file)){
		stop('glm_nc file must be supplied string or proper file handle')
	}
	glm_nc	<- 	nc_open(file, readunlim=TRUE)
	return(glm_nc)
}
#' @importFrom ncdf4 nc_close
close_glm_nc <- function(glm_nc){
  nc_close(glm_nc)
}


# Summary: Returns the converted time vector in R format
#' @importFrom ncdf4 ncvar_get
get_time  <-  function(glm_nc){
	hours_since  <- as.numeric(ncvar_get(glm_nc, "time"))
	time_info <- get_time_info(glm_nc)

	time <- time_info$startDate + time_info$time_unit * hours_since * 60*60*24

	return(time)
	
}

#' @importFrom ncdf4 ncatt_get ncvar_get
get_time_info <- function(glm_nc, file = NULL){
  
	day_secs = 86400
  time_unit <- 3600/day_secs
	close_nc <- FALSE #flag if we should close nc in this function
	
	#The units attribute on the time variable has basically the info we need
  if (missing(glm_nc)){
    glm_nc <- get_glm_nc(file)
    close_nc <- TRUE
  }
	time_units <- ncatt_get(glm_nc,'time','units')$value

	
	#It is written in prose instead of machine-readable format. Check to makes sure
	# it says "hours since ", then we know the timestep is hours. As far as I know, 
	# this never changes
	tiCheck <- regexpr('(hours since) (.*)' ,time_units, perl=TRUE)

	#make sure the unit string is as expected. I think 
	# the timestep is always in hours
	if(attr(tiCheck,'capture.start')[1] < 0 || attr(tiCheck,'capture.start')[2] < 0){
		stop('Unexpected time unit in NetCDF file')
	}

	# Get the epoch from the unit string
	epoch <- substr(time_units, attr(tiCheck,'capture.start')[2], attr(tiCheck,'capture.start')[2] + attr(tiCheck,'capture.length')[2])

	#get the length of the time data, will use this later
	tLen <- glm_nc$dim[["time"]][["len"]]
	
  time_info  <-  data.frame("time_unit"=time_unit)
  start_date <- coerce_date(epoch)
  time_info  <-  cbind(time_info,"startDate"=start_date)

	#End date/time 
	endT <- time_info$startDate + ncvar_get(glm_nc, 'time', start=tLen, count=1) * time_unit * day_secs

  time_info  <-  cbind(time_info,"stopDate"=endT[1])
	if (close_nc){
	  close_glm_nc(glm_nc)
	}
	return(time_info)
}

.is_heatmap <- function(file, var_name){
  
  glm_nc <- get_glm_nc(file)
  in.nc <- var_name %in% names(glm_nc$var)
  if (!all(in.nc)){
    close_glm_nc(glm_nc)
    stop(paste(var_name[!in.nc], collapse=', '),' not in ', file)
  }
      
  
  dims <-unlist(lapply(X = var_name, FUN = function(x) length(glm_nc$var[[x]]$dim)))
  
  close_glm_nc(glm_nc)
  #dim == 4 is heatmap (3D)
  return(dims==4 | dims == 0)
}

