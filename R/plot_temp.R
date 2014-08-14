#'@title plot water temperatures from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@keywords methods
#'@seealso \link{get_temp}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'plot_temp(file = file)
#'@export
plot_temp <- function(file){
  
  get_temp <-  function(file, reference = 'surface', z_out, t_out = NULL)
  palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                              bias = 1, space = "rgb")
  filled.contour(wtr.dates, y, wtr.mat, ylim=c(max(depths),0), nlevels=100,
                 color.palette= palette, ylab='Depths (m)') #Sets range and value of color hues
}