#'Get volumetrically averaged whole lake temperature
#'@details A metric function used like an rLakeAnalyzer function to be 
#'called by \code{\link{sim_metrics}}
#'@param wtr a numeric vector of water temperature in degrees C.
#'@param depths a numeric vector corresponding to the depths (in m) of the wtr measurements
#'@param bthA a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
#'@param bthD a numeric vector of depths (m) which correspond to areal measures in bthA
#'
#'@importFrom rLakeAnalyzer layer.temperature
#'
#'@seealso \code{\link{sim_metrics}}, \code{\link{compare_to_field}}, \code{\link{validate_sim}}
#'@export
whole.lake.temperature <- function(wtr, depths, bthA, bthD){
	
	
	avg_temp = layer.temperature(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	
	return(avg_temp)
}
