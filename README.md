glmtools
=====
[![Build status](https://ci.appveyor.com/api/projects/status/j5mscylmkssa0esf)](https://ci.appveyor.com/project/jread-usgs/glmtools) 
[![Build Status](https://travis-ci.org/USGS-R/glmtools.svg)](https://travis-ci.org/USGS-R/glmtools)
[![Coverage Status](https://img.shields.io/coveralls/USGS-R/glmtools.svg)](https://coveralls.io/r/USGS-R/glmtools)
Tools for interacting with the [General Lake Model (GLM)](http://aed.see.uwa.edu.au/research/models/GLM/ "General Lake Model's website") in R. `glmtools` includes some basic functions for calculating physical derivatives and thermal properties of model output, and some plotting functionality (see example image below). 


`glmtools` Functions (as of v0.2.5.2)
=====
| Function       | Title           |
| ------------- |:-------------|
| `compare_to_field` | compare metric for GLM vs field observations |
| `get_evaporation`  | get evaporation from GLM simulation |
| `get_hypsography` | retrieve hypsography information |
| `get_ice` | get ice depth from GLM simulation |
| `get_nml_value` | gets a nml value according to an arg_name |
| `get_surface_height` | get surface height from GLM simulation |
| `get_temp` | get water temperatures from a GLM simulation |
| `get_wind` | get wind speed from GLM simulation |
| `model_diagnostics` | run diagnostics on model results |
| `plot_temp` | plot water temperatures from a GLM simulation |
| `read_field_obs` | read in field data into a data.frame |
| `read_nml` | read in a GLM simulation *.nml file |
| `resample_sim` | get subset of time from a generic timeseries data.frame |
| `resample_to_field` | match GLM water temperatures with field observations |
| `set_nml` | sets values in nml object |
| `sim_metrics` | get possible metrics for comparing GLM outputs to field |
| `summarize_sim` | creates GLM simulation summary outputs |
| `validate_sim` | run diagnostics on model results vs observations |
| `write_nml` | write GLM *.nml for a GLM simulation |

FAQ
=====

How do I install `glmtools`? It isn't on CRAN!
===
`glmtools` can be installed from the [USGS GRAN](http://owi.usgs.gov/R/gran.html) repository. To install `glmtools` and all dependencies, follow the appropriate instructions below:

1. Setup R to add GRAN using the instructions on the [USGS R page](http://owi.usgs.gov/R/gran.html).

2. After GRAN is setup (usually requires a restart of R), then use the following command in R:

    `install.packages("glmtools")`
        
What libraries does `glmtools` need?
===
This version requires the NetCDF version R library (called `ncdf4`), and `rLakeAnalyzer`, and `tools` if you would like to run all examples. 

How do I contribute new code back to the glmtools project?
===

In order to contribute to this code, we recommend the following workflow: 

1) "fork" this repository to your own personal github account

2) clone the github repository to your computer:

	$git clone https://github.com/{username}/glmtools.git

3) modify code or add new functionality, save the code

4) add the repository master to a remote master called "upstream"

	$cd glmtools

	$git remote add upstream https://github.com/USGS-R/glmtools.git

5) before pushing your changes to your repository, pull in the current version of the GLEON master:

	$git fetch upstream

6) merge these differences with your own "master" version:

	$git merge upstream/master

7) push your changes to your github repository, in addition to changes made by pulling in the GLEON master:

	$git push

8) submit a pull request to GLEON master using your account at github.com

