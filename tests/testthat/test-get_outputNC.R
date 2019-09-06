context("get_wind")

test_that("get_wind", {
  sim_folder <- run_example_sim(verbose = F)
  nc_file <<- file.path(sim_folder, 'output/output.nc')
  wnd <- get_wind(nc_file)
  t_out <- as.POSIXct(c("1900-01-01","1900-01-02"))
  
  #should be empty
  expect_warning(get_wind(nc_file, t_out = t_out, method = 'interp'))
  
  t_out <- as.POSIXct(c("2010-04-16 08:15", 
                        "2010-06-14 10:30", "2010-06-25 10:21", 
                        "2010-07-28 10:00"),tz = 'GMT') 
  
  df <- get_wind(nc_file, t_out = t_out, method = 'interp', precision = 'exact')
  expect_true(all(t_out == df[,1]))
})

# test_that("check hanging nml", {
#   expect_error(read_nml(system.file(package='glmtools', 'extdata', 'hanging_line.nml')), "Empty values after")
# })

context("get_surface_height")

test_that("get_surface_height", {
  wnd <- get_surface_height(nc_file)
  t_out <- as.POSIXct(c("1900-01-01","1900-01-02"))
  
  #should be empty
  expect_warning(get_surface_height(nc_file, t_out = t_out, method = 'interp'))
  
  t_out <- as.POSIXct(c("2010-04-16 08:15", 
                        "2010-06-14 10:30", "2010-06-25 10:21", 
                        "2010-07-28 10:00"),tz = 'GMT') 
  
  df <- get_surface_height(nc_file, t_out = t_out, method = 'interp', precision = 'exact')
  expect_true(all(t_out == df[,1]))
})

context("get_ice")

test_that("get_ice",{
  t_out <- as.POSIXct(c("1900-01-01","1900-01-02"))
  expect_warning(get_ice(nc_file, t_out = t_out, method = 'interp'))
  expect_warning(get_ice(nc_file, t_out = t_out, method = 'interp', snow.rm = FALSE))
})

context("sim_vars")

test_that("get variables from sim",{
  sim_folder <- run_example_sim(verbose = FALSE)
  nc_file <- file.path(sim_folder, 'output/output.nc')
  expect_is(sim_vars(file = nc_file), 'data.frame')
  expect_is(sim_var_longname(nc_file, 'u_mean'), 'character')
  expect_is(sim_var_units(nc_file, 'u_mean'), 'character')
  expect_error(sim_var_units(nc_file, 'u_meanBADNAME'))
})

context(".is_heatmap")

test_that("test is heatmap",{
  sim_folder <- run_example_sim(verbose = FALSE)
  nc_file <- file.path(sim_folder, 'output/output.nc')
  var_names <- c("hice","NS","garbage","other")
  expect_error(glmtools:::.is_heatmap(nc_file, var_names), 'garbage, other not in ')
  expect_is(glmtools:::.is_heatmap(nc_file, c('NS','hice')), 'logical')
  expect_true(glmtools:::.is_heatmap(nc_file,'temp'))
  expect_false(glmtools:::.is_heatmap(nc_file,'evap'))
})