
#' convert an existing simulation variable into a different one
#' 
#' allows you to provide specific variable or unit conversions to create a new variable 
#' that can be used with \code{\link{plot_var_compare}}, and others. 
#' 
#' @param nc_file the netcdf output file
#' @param \dots an expression to convert variable based on other variables or offsets
#' @param unit the units for the new variable
#' @param longname the longname for the new variable
#' @export
#' @importFrom lazyeval lazy_dots lazy_eval
#' @importFrom ncdf4 ncvar_def ncvar_put ncvar_add
#' @examples 
#' \dontrun{
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'convert_sim_var(nc_file, tempF = temp/5*9+32, unit='degF',longname='temperature degrees Farenheit')
#'plot_var(nc_file, 'tempF')
#'convert_sim_var(nc_file, crazy_var = temp-u_mean*1000)
#'plot_var(nc_file, 'crazy_var')
#'
#'temp2f <- function(c) c/5*9+32
#'convert_sim_var(nc_file, tempf = temp2f(temp), unit='degF',longname='temperature degrees Farenheit')
#' }
#' 
convert_sim_var <- function(nc_file='output.nc', ..., unit='', longname='', 
                            overwrite=FALSE){

  sim.vars <- sim_vars(nc_file)$name
  message('convert_sim_var is untested and in development')
  
  # // here, vals would be defined by the function passed in by `...`. Probably captured w/ lazyeval?
  # lazyeval::lazy_eval(convert, data=list(temp=ncvar_get(nc, 'temp')))
  convert <- lazyeval::lazy_dots(...)
  if (length(convert) > 1)
    stop('not yet ready to handle multi-var expressions')
  
  var.name <- names(convert)
  var.exists <- var.name %in% sim.vars
  if (var.exists & !overwrite)
    stop(var.name, ' cannot be added, it already exists and overwrite = FALSE.', call. = FALSE)
  
  fun.string <- deparse(convert[[1]]$expr)
  variables <- sim.vars[sapply(sim.vars,grepl, x = fun.string)]
  nc.vars <- variables[variables %in% sim.vars]
  
  data <- lapply(nc.vars, function(v) get_raw(nc_file, v))
  names(data) <- nc.vars
  vals <- lazyeval::lazy_eval(convert, data=data)[[1]]
  
  nc <- nc_open(nc_file, readunlim=TRUE, write=TRUE)
  
  
  if (!var.exists){
    #depending on conversion, dims can be [time], [lon,lat,time], or [lon,lat,z,time] 
    lon <- nc$dim$lon
    lat <- nc$dim$lon
    time <- nc$dim$time
    if (length(dim(vals)) > 1){
      z <- nc$dim$z
      dim = list(lon,lat,z,time)
    } else {
      dim = list(lon,lat,time)
    }
    
    missval = 9.96920996838687e36
    var_new <- ncvar_def(name=var.name, unit, dim = dim, missval, prec="double")
    nc <- ncvar_add(nc, var_new)
  } else {
    var_new <- var.name
  }
    
  
  ncvar_put(nc, var_new, vals=vals)
  nc_close(nc)
}