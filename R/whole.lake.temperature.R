#'@title Get volumetrically averaged whole lake temperature
#'@details A metric function used like an rLakeAnalyzer function to be 
#'called by \code{\link{sim_metrics}}
#'@param wtr a water temperature vector
#'
#'@import rLakeAnalyzer
#'
#'@seealso \code{\link{sim_metrics}}, \code{\link{compare_to_field}}, \code{\link{validate_sim}}
#'@export
whole.lake.temperature <- function(wtr, depths, bthA, bthD){
	
	
	avg_temp = layer.temperature(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	
	return(avg_temp)
}
