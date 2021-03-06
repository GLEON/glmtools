% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_var_nc.R
\name{plot_var_nc}
\alias{plot_var_nc}
\alias{plot_var}
\title{Plot variables from a .nc file}
\usage{
plot_var_nc(
  nc_file = "output.nc",
  var_name = "temp",
  fig_path = NULL,
  reference = "surface",
  legend.title = NULL,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  plot.title = NULL,
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = NULL,
  ...
)

plot_var(
  nc_file = "output.nc",
  var_name = "temp",
  fig_path = NULL,
  reference = "surface",
  legend.title = NULL,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  plot.title = NULL,
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = NULL,
  ...
)
}
\arguments{
\item{nc_file}{a string with the path to the netcdf output from GLM}

\item{var_name}{a character vector of valid variable names (see \code{\link{sim_vars}})}

\item{fig_path}{Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples.}

\item{reference}{String; 'surface' or 'bottom'. Only used for heatmap plots. We recommend using 'bottom' if surface levels are fluctuating. This will present a more realistic representation of surface conditions.}

\item{legend.title}{Vector string; Default (`NULL`) will use variable and units from netcdf file}

\item{interval}{Positive number indicating the depth interval in meters to interpolate output data. Must be less than max depth of lake. Default = 0.5 m.}

\item{text.size}{Integer; Default is 12. Higher values will increase text size in plot.}

\item{show.legend}{Logical; TRUE to show legend (default), FALSE to hide legend}

\item{legend.position}{String; Legend position. Default is 'right'. Options: 'left','right','top','bottom'}

\item{plot.title}{Vector string; Default is no title.}

\item{color.palette}{See \code{\link[ggplot2:scale_color_distiller]{ggplot2:scale_color_distiller}} . If a string, will use that named palette. Default is 'RdYlBu'. If a number, will index into the list of palettes of appropriate. 
Palettes available include: Diverging:
BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn. Spectral. Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3. Sequential:
Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd.}

\item{color.direction}{Sets the order of colors in the scale. If 1, colors are as output by brewer.pal. If -1, the order of colors is reversed (default).}

\item{zlim}{Color palette limits for z-variable. Default is maximum range of variable. Set as c(value,value).}

\item{...}{additional arguments passed to \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}}
}
\description{
Plots variables directly from a .nc file output from a GLM simulation. Replaces function plot_var.
}
\section{Functions}{
\itemize{
\item \code{plot_var}: Deprecated. Use plot_var_nc
}}

\examples{
nc_file <- system.file("extdata", "output/output.nc", package = "glmtools")
plot_var_nc(nc_file, 'temp')

#Plotting two variables
plot_var_nc(nc_file, var_name = c('temp','wind'), show.legend = FALSE, 
text.size = 14, plot.title = c('My Lake: Temp','My Lake: Wind'))

#Change color palette
plot_var_nc(nc_file, var_name = 'temp', color.palette = 'PuBuGn', 
color.direction = 1, show.legend = FALSE)

\dontrun{
#'#How to save a plot
plot_var_nc(nc_file,var_name = c('temp', 'u_mean'),
fig_path = './figtest.png', width = 6, height = 2, units = 'in')

# need to specify a valid .nc file here: 
plot_var_nc(file = fabm_sim_nc.nc,
var_name = 'aed_oxygen_oxy', 
fig_path = 'aed_out.png')
}
}
\seealso{
\code{\link{get_temp}}, \code{\link{sim_var_longname}}, 
\code{\link{sim_vars}}, \code{\link{plot_temp}},  \code{\link{get_var}}
}
\author{
Jordan S. Read, Luke A. Winslow, Hilary A. Dugan
}
\keyword{methods}
