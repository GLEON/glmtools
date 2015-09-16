context("aed nmls can be parsed")

test_that("normal aed parse works",{
  
  aed_nml <- read_nml(system.file('extdata','fabm.nml', package='glmtools'))
  
  expect_equal(get_nml_value(aed_nml, 'num_tracers'), 1)
})

test_that("aed block::arg_name works",{
  
  aed_nml <- read_nml(system.file('extdata','fabm.nml', package='glmtools'))
  
  expect_equal(get_nml_value(aed_nml, 'aed_oxygen::Fsed_oxy'), -50)
  expect_equal(get_nml_value(aed_nml, 'aed_sed_constant::Fsed_oxy'), -3)
  
})
