#'@title get units of variable from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param var_name name of a valid variable from a simulation (see \code{\link{sim_vars}})
#'@keywords methods
#'@seealso \code{\link{sim_vars}}, \code{\link{sim_var_longname}}
#'@author
#'Jordan S. Read
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'vars <- sim_vars(file = nc_file)
#'sim_var_units(nc_file, vars[1,]$name)
#'sim_var_units(nc_file, 'u_mean')
#'@export
sim_var_units <- function(file='output.nc', var_name){
  
  glm_nc <- get_glm_nc(file)
  units <- glm_nc$var[[var_name]]$units
  close_glm_nc(glm_nc)
  if (is.null(units)){stop(var_name,' not found. Check variable name')}
  return(units)
}



