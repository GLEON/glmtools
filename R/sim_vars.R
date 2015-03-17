#'@title get list of variables from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@keywords methods
#'@seealso \code{\link{sim_var_longname}}, \code{\link{sim_var_units}}, \code{\link{plot_var}}
#'@return 
#'A data frame with columns \code{name, unit, longname}. 
#'@author
#'Stuart E. Jones, Jordan S. Read, Luke A. Winslow
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'sim_vars(file = nc_file)
#'@export
sim_vars <- function(file){
  
  glm_nc <- get_glm_nc(file)
  #get the list of variable names
  var_list = unlist(lapply(glm_nc$var,function(x){x$name}))
  
  #build a data table and populate with units and longnames
  out = data.frame(name=unname(var_list))
  out$unit     = unlist(lapply(glm_nc$var,function(x){x$units}))
  out$longname = unlist(lapply(glm_nc$var,function(x){x$longname}))
  
  #Return in alphabetical order by name
  out = out[order(out$name),]
  row.names(out) = 1:nrow(out)
  
  close_glm_nc(glm_nc)
  
  return(out)
}

