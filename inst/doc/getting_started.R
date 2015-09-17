## ----setup, include=FALSE---------------------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)


## ---- eval=FALSE------------------------------------------
#  install.packages("glmtools",
#      repos = c("http://owi.usgs.gov/R"),
#      dependencies = TRUE)

## ---------------------------------------------------------
library(glmtools)
eg_folder = system.file('extdata', package = 'GLMr')
print(eg_folder) # location of example data

## ---------------------------------------------------------
dir(eg_folder)

## ---- eval=FALSE------------------------------------------
#  glm_version()

## ---------------------------------------------------------
citation('GLMr')

## ---- eval=FALSE------------------------------------------
#  source("http://owi.usgs.gov/R/add_gran_repo.R")
#  update.packages()

## ---- eval=FALSE------------------------------------------
#  update.packages(repos = c("http://owi.usgs.gov/R"))

## ---- results ='hide'-------------------------------------
run_glm(eg_folder)

## ---------------------------------------------------------
dir(eg_folder)

## ---- eval =FALSE-----------------------------------------
#  run_glm('~/Documents/my_sim')

## ---------------------------------------------------------
eg_nml <- read_nml(nml_file = file.path(eg_folder,'glm2.nml'))

## ---------------------------------------------------------
eg_nml

## ---------------------------------------------------------
class(eg_nml)
names(eg_nml)
eg_nml[[1]][1:4]

## ---------------------------------------------------------
# water clarity
get_nml_value(eg_nml, 'Kw')

## ---------------------------------------------------------
# initial conditions for depths
get_nml_value(eg_nml, 'the_depths')

## ---------------------------------------------------------
# water clarity
eg_nml <- set_nml(eg_nml, 'Kw', 1.4)
# note how the value is now changed:
get_nml_value(eg_nml, 'Kw')

## ---------------------------------------------------------
eg_nml <- set_nml(eg_nml, arg_list = list('Kw' = 1.2, 'max_layers' = 480))
get_nml_value(eg_nml, 'max_layers')

## ---------------------------------------------------------
# define a location for the file to be written to. Here it will overwrite the existing `nml` file:
write_path <- file.path(eg_folder,'glm2.nml')
write_nml(eg_nml, file = write_path)

## ---------------------------------------------------------
run_glm(eg_folder)

## ---- fig.width=4, fig.height=2.5-------------------------
nc_file <- file.path(eg_folder, 'output.nc')
plot_temp(file = nc_file)

## ---------------------------------------------------------
sim_vars(file = nc_file)
plot_var(file = nc_file, var_name = c('temp','u_mean'))

## ---------------------------------------------------------
sim_folder <- run_example_sim(verbose = FALSE)
nc_file <- file.path(sim_folder, 'output.nc')
field_file <- file.path(sim_folder, 'field_data.tsv')


## ---------------------------------------------------------
thermo_values <- compare_to_field(nc_file, field_file,
                          metric = 'thermo.depth', as_value = TRUE)

## ---------------------------------------------------------
temp_rmse <- compare_to_field(nc_file, field_file,
                          metric = 'water.temperature', as_value = FALSE)
print(paste(temp_rmse,'deg C RMSE'))

## ---- fig.width=6, fig.height=5.25------------------------
sim_folder <- run_example_sim(verbose = FALSE)
nc_file <- file.path(sim_folder, 'output.nc')
nml_file <- file.path(sim_folder, 'glm2.nml')
field_file <- file.path(sim_folder, 'field_data.tsv')

plot_temp_compare(nc_file, field_file) ##makes a plot!

## ---- fig.width=4, fig.height=3.5-------------------------
field_file <- file.path(sim_folder, 'field_stage.csv')

plot_compare_stage(nc_file, field_file) ##makes a plot!

