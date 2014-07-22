#'@title get ice depth from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and ice.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param reference a string which specifies the vertical reference ('surface' or 'bottom')
#'@param z_out a vector of depths for temperature output (in meters)
#'@return a data.frame with DateTime and temperature at depth 
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'file = '../test/output.nc'
#'temp_surf <- get_temp(file,reference='surface',z_out=c(0,1,2))
#'temp_bot <- get_temp(file,reference='bot',z_out=c(0,1,2))
#'@export
get_temp <-  function(file, reference='bottom', z_out){
  
  glm_nc <- get_glm_nc(file)
  if (reference!='bottom' & reference!='surface'){
    stop('reference input must be either "surface" or "bottom"')
  }
  
  tallest_layer <- ncvar_get(glm_nc, "NS") #The last useful index
  elev <- ncvar_get(GLMnc, "z" )
  temp <- ncvar_get(GLMnc, "temp")
  time <- get_time(glm_nc)
  if (reference=='surface') elev_surf = get_surface_height(file)
  nc_close(glm_nc)
  
  max_i <- max(tallest_layer)
  # rows are layers, columns are time..  
  if (length(dim(elev))==2){
    elev	<-	elev[1:max_i, ] 
    temp 	<-	temp[1:max_i, ]
  } else {
    if (dim(elev)==0){stop('empty nc file')}
    else {
      elev	<-	elev[1:max_i]
      temp 	<-	temp[1:max_i]
    }
  }
  
  
  #No temperature or elevation should be > 1e30, should be converted to NA
  rmvI	<- 	which(temp>=1e30 | elev>=1e30)
  elev[rmvI]	<- NA
  temp[rmvI]	<- NA
  num_step	<-	length(time)
  num_dep	<-  length(z_out)
  
  temp_out <- matrix(nrow=num_step,ncol=num_dep) # pre-populated w/ NAs
  if (is.null(nrow(temp))){ # handle single depth layer of model
    for (tme in 1:num_step){
      if (reference == 'surface') elevs_out <- elev_surf[tme, 2] - z_out
      temp_out[tme, ] <- depth_resample(elevs=elev[tme], temps=temp[tme], elevs_out)
    }
  } else { 
    for (tme in 1:num_step){
      if (reference == 'surface') elevs_out <- elev_surf[tme, 2] - z_out
      temp_out[tme, ] <- depth_resample(elevs=elev[, tme], temps=temp[, tme], elevs_out)
    }
  }
  glm_temp <- data.frame(time)
  glm_temp <- cbind(glm_temp,temp_out)
  frameNms	<-	letters[seq( from = 1, to = num_dep )]
  frameNms[1] <- "DateTime"
  
  for (z in 1:numDep){
    out_head <- ifelse(reference=='surface', 'wtr_', 'elv_')
    frameNms[z+1]  <- paste(c(out_head,as.character(z_out[z])),collapse="")
  }
  names(glm_temp)<- frameNms

  return(glm_temp)
}

# private?
subsampleGLM	<-	function(GLM, sampleTime, sampleDepths){
  
  # sample at depths of 'sampleElev' at time 'sampleDepths'
  glmElev	<-	getElevGLM(GLM)
  surfaceElevs <-  getSurfaceElevGLM(GLM)
  dates	<-	GLM$DateTime
  times	<-	as.POSIXct(sampleTime)
  output = matrix(NaN, nrow=length(sampleTime), ncol=length(sampleDepths))
  
  for (i in 1:length(times)){
    time = times[i]
    diffs	<-	abs(dates-time)
    uIndx	<-	which.min(diffs)	# NEED good way to interpolate temporally to get exact time...
    
    
    interpElevs	<-	surfaceElevs[uIndx]-sampleDepths	# now are elevations
    drops <- c("DateTime")
    temp <- as.numeric(GLM[uIndx,!(names(GLM) %in% drops)])
    
    if (length(temp[!is.na(temp)])>1){
      wtr	<-	approx(glmElev,temp,xout=interpElevs)
    } else {
      wtr	<-	data.frame(y=interpElevs*NA)
    }
    
    
    if (as.numeric(diffs[uIndx])<24){
      output[i,] = wtr$y
    }
    
  }
  
  return(output)
}


depth_resample <- function(elevs, temps, elevs_out){
  #strip out NAs
  rmv_i <- is.na(elevs)
  elevs <- elevs[!rmv_i]
  temps <- temps[!rmv_i]
  num_z <- length(elevs)
  layer_mids <- c(elevs[1]/2, elevs[1:num_z-1] + diff(elevs)/2)
  temps_re <- c(temps[1], temps, tail(temps,1))
  elevs_re <- c(0, layer_mids, tail(elevs, 1))
  
  temps <- approx(x = elevs_re, y = temps_re, xout = elevs_out)$y
  return(temps)
}

