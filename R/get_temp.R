#'@title get water temperatures from a GLM simulation
#'@description 
#'Creates a data.frame with DateTime and temperatures (in deg C).  \cr
#'Temperatures that are sampled out of the GLM output are taken relative 
#'to the surface (\code{reference = 'surface'}) or the bottom of the lake 
#'(\code{reference = 'bottom'}).
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param reference a string which specifies the vertical reference ('surface' or 'bottom')
#'@param z_out an optional vector of depths for temperature output (in meters). 
#'If NULL, depths will be determined based on the depth of the lake
#'@param t_out a vector of POSIXct dates for temporal resampling (order is important)
#'@return a data.frame with DateTime and temperature at depth 
#'@keywords methods
#'@seealso \link{resample_sim}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'temp_surf <- get_temp(file, reference = 'surface', z_out = c(0,1,2))
#'temp_bot <- get_temp(file, reference = 'bottom', z_out = c(0,1,2))
#'temp_bot <- get_temp(file)
#'
#'#-- get temporal subset--
#'t_out <- seq(as.POSIXct("2011-04-04"), as.POSIXct("2011-06-01"), by = 86400)
#'temp_surf <- get_temp(file, reference = 'surface', z_out = 0, t_out = t_out)
#'plot(temp_surf)
#'@import ncdf4
#'@export
get_temp <-  function(file, reference = 'bottom', z_out = NULL, t_out = NULL){
  if (reference!='bottom' & reference!='surface'){
    stop('reference input must be either "surface" or "bottom"')
  }

  if (is.null(z_out)){
    mx_lyrs <- 20
    z_lyrs <- seq(0,mx_lyrs-1) # default layers
    z_out = (max(get_surface_height(file)[, 2], na.rm = TRUE)/tail(z_lyrs,1)) * z_lyrs
  }
  glm_nc <- get_glm_nc(file)
  
  tallest_layer <- ncvar_get(glm_nc, "NS") #The last useful index
  elev <- ncvar_get(glm_nc, "z" )
  temp <- ncvar_get(glm_nc, "temp")
  time <- get_time(glm_nc)
  
  nc_close(glm_nc)
  
  if (reference=='surface') {
    elev_surf = get_surface_height(file)
  } else {
    elevs_out = z_out
  }
  
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

  num_step	<-	length(time)
  num_dep	<-  length(z_out)
  
  temp_out <- matrix(nrow=num_step,ncol=num_dep) # pre-populated w/ NAs
  
  for (tme in 1:num_step){
    if (reference == 'surface') elevs_out <- elev_surf[tme, 2] - z_out
    if (is.null(nrow(temp))){ # handle single depth layer of model
      temp_out[tme, ] <- resample_depth(elevs=elev[tme], temps=temp[tme], elevs_out)
    } else {
      temp_out[tme, ] <- resample_depth(elevs=elev[, tme], temps=temp[, tme], elevs_out)
    }
    
  }
  
  glm_temp <- data.frame(time)
  glm_temp <- cbind(glm_temp,temp_out)
  frameNms	<-	letters[seq( from = 1, to = num_dep )]
  frameNms[1] <- "DateTime"
  
  for (z in 1:num_dep){
    out_head <- ifelse(reference=='surface', 'wtr_', 'elv_')
    frameNms[z+1]  <- paste(c(out_head,as.character(z_out[z])),collapse="")
  }
  names(glm_temp)<- frameNms

  glm_temp <- resample_sim(glm_temp, t_out)
  
  return(glm_temp)
}


resample_depth <- function(elevs, temps, elevs_out){
  #strip out NAs
  rmv_i <- temps>=1e30 | elevs>=1e30
  elevs <- elevs[!rmv_i]
  temps <- temps[!rmv_i]
  num_z <- length(elevs)
  layer_mids <- c(elevs[1]/2, elevs[1:num_z-1] + diff(elevs)/2)
  temps_re <- c(temps[1], temps, tail(temps,1))
  elevs_re <- c(0, layer_mids, tail(elevs, 1))
  
  temps <- approx(x = elevs_re, y = temps_re, xout = elevs_out)$y
  return(temps)
}

