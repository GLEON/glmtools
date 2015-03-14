#'@title get list of variables from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@keywords methods
#'@seealso \code{\link{sim_var_longname}}, \code{\link{sim_var_units}}, \code{\link{plot_var}}
#'@author
#'Stuart E. Jones, Jordan S. Read, Luke A. Winslow
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'sim_vars(file = nc_file)
#'@export
sim_vars <- function(file){
  
  glm_nc <- get_glm_nc(file)
  var_list <- sort(unlist(lapply(glm_nc$var,function(x){x$name})))
  names(var_list)=NULL
  
  close_glm_nc(glm_nc)
  return(var_list)
}

