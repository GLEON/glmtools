#'@title get list of variables from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@keywords methods
#'@author
#'Jordan S. Read, Luke A. Winslow, Stuart E. Jones
#'@examples
#'\dontrun{
#'nc_file<-system.file('extdata','output.nc',package='glmtools')
#'sim_vars(file = nc_file)
#'}
#'@export
sim_vars <- function(file){
  
  glm_nc <- get_glm_nc(file)
  var_list <- sort(unlist(lapply(glm_nc$var,function(x){x$name})))
  names(var_list)=NULL
  
  close_glm_nc(glm_nc)
  return(var_list)
}

