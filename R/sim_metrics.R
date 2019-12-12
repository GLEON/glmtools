#'@title get possible metrics for comparing GLM outputs to field
#'@description
#'returns a character array of metric names that can be supported by \code{compare_to_field}. 
#'This function is only designed to handle calls to physical metrics that return a single value.
#'An example of this behavior is thermocline depth (but not water density for all depth-resolved measurements).
#'@param with_nml include metrics that require information in the nml file
#'@return a character array of metrics that can be used
#'@keywords methods
#'@seealso \link{compare_to_field}, \link{resample_to_field}, \link{read_nml}
#'@author
#'Jordan S. Read
#'@examples 
#'sim_metrics()
#'sim_metrics(with_nml = TRUE)
#'@import rLakeAnalyzer
#'@importFrom methods getPackageName
#'@export
sim_metrics <- function(with_nml = FALSE){
  package_name <- 'rLakeAnalyzer'
  funs <- fun_from_package(package_name)
  funs <- c(funs, fun_from_package(getPackageName(), private = TRUE))
  arg_names <- c('wtr', 'depths')
  
  
  if (with_nml){
    arg_names = c('wtr', 'depths', 'bthA', 'bthD')
    metrics <- match_inputs(funs, arg_names)
  } else {
    metrics <- match_inputs(funs, arg_names)
  }
  
  
  return(metrics)
}

#' @importFrom utils lsf.str 
fun_from_package <- function(package_name, private = FALSE){
  
  if (private){
    funs <- as.character(unclass(lsf.str(envir = getNamespace(package_name), 
                                         all = TRUE)))
  } else {
    funs <- ls(paste0('package:', package_name)) # get public functions
  }
  
  return(funs)
}

match_inputs <- function(funs, arg_names){
  
  ignore_args = c('ts', 'plot')
  
  rmv_i <- vector(length=length(funs))
  for (i in 1:length(ignore_args)){
    rmv_i <- rmv_i | grepl(ignore_args[i],funs)
  }
  
  funs <- funs[!rmv_i]
  
  fun_valid <- vector(length = length(funs))
  for (i in 1:length(funs)){
    all_args <- names(formals(funs[i]))
    def_args <- names(Filter(function(x) !identical(x, quote(expr = )), formals(funs[i])))
    req_args <- all_args[!all_args %in% def_args]
    
    fun_valid[i] = all(req_args %in% arg_names) & 
      length(req_args) > 0 # case where all are default
    if (fun_valid[i]){
      o_dim <- output_dim(funs[i])
      if (o_dim == 2){fun_valid[i]=FALSE} # case where it is a multi-out like metadepths. Could limit this to == 1 and handle temp and density differently
    }
      
  }
  
  valid_funs <- funs[fun_valid]

  return(valid_funs)

}

output_dim <- function(fun){
  # probably a better way to test for dimension of output, but this works..
  bthA  <-	c(1000,900,864,820,200,10)
  bthD	<-	c(0,2.3,2.5,4.2,5.8,7)
  
  wtr	<-	c(28,27,26.4,26,25.4,24,23.3)
  depths	<-	c(0,1,2,3,4,5,6)
  
  arg_list <- list(wtr=wtr, depths = depths, bthA = bthA, bthD = bthD)
  use_names <- names(arg_list) %in% names(formals(fun))
  out <- do.call(get(fun), arg_list[use_names])
  return(length(out))
  
}