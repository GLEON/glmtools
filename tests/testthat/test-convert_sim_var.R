context("convert variables in netcdf output")

test_that("errors with poor matches", {
  sim_folder <- run_example_sim(verbose = FALSE)
  nc_file <<- file.path(sim_folder, 'output.nc')
  expect_error(convert_sim_var(nc_file, temp = temp*2), "temp cannot be added, it already exists.")
  expect_error(convert_sim_var(nc_file, temp.new = garbage))
})

test_that("can add variable", {
  convert_sim_var(nc_file, temp.new = temp*2)
  expect_true('temp.new' %in% sim_vars(nc_file)$name)
  expect_is(get_var(nc_file, var_name = 'temp.new'), 'data.frame')
})

test_that("errors when you try to add it again", {
  expect_error(convert_sim_var(nc_file, temp.new = temp*2))
})

test_that("can modify variable with more than one variable function", {

  convert_sim_var(nc_file, crazy_var = temp-u_mean*1000)
  expect_true('crazy_var' %in% sim_vars(nc_file)$name)
})

test_that("can modify variable with a function", {
  
  temp2f <- function(c) c/5*9+32
  convert_sim_var(nc_file, tempf = temp2f(temp), unit='degF',longname='temperature degrees Farenheit')
  expect_true('tempf' %in% sim_vars(nc_file)$name)
  expect_error(convert_sim_var(nc_file, tempf2 = garbagefun(temp)))
  
})
