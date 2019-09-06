context("run example simulation")

test_that("running glm simulation", {
  sim_folder <- run_example_sim(verbose = F)
  # check that file exists
  nml <- read_nml(nml_file = file.path(sim_folder,'glm3.nml'))
  testthat::expect_is(nml, 'nml')
  
  testthat::expect_true(file.exists(file.path(sim_folder,'output/output.nc')))
  
})