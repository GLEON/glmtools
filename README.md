glmtools
=====
[![Build status](https://ci.appveyor.com/api/projects/status/j5mscylmkssa0esf/branch/master)](https://ci.appveyor.com/project/jread-usgs/glmtools/branch/master)  
Tools for interacting with the [General Lake Model (GLM)](http://aed.see.uwa.edu.au/research/models/GLM/ "General Lake Model's website") in R. `glmtools` includes some basic functions for calculating physical derivatives and thermal properties of model output, and some plotting functionality (see example image below).

![alt tag](http://github.gleon.io/images/test_figure.png)

`glmtools` Functions (as of v0.2.3)
=====
| Function       | Title           |
| ------------- |:-------------|
| `compare_to_field` | compare metric for GLM vs field observations |
| `get_evaporation`  | get evaporation from GLM simulation |
| `get_hypsography` | retrieve hypsography information |
| `get_ice` | get ice depth from GLM simulation |
| `get_metrics` | get possible metrics for comparing GLM outputs to field |
| `get_nml_value` | gets a nml value according to an arg_name |
| `get_surface_height` | get surface height from GLM simulation |
| `get_temp` | get water temperatures from a GLM simulation |
| `get_wind` | get wind speed from GLM simulation |
| `init_lke` | initializes Lake Analyzer list for .lke file |
| `plot_temp` | plot water temperatures from a GLM simulation |
| `pretty_nml` | prints R nml list with pretty formatting |
| `read_field_obs` | read in field data into a data.frame |
| `read_nml` | read in a GLM simulation *.nml file |
| `resample_time` | get subset of time from a generic timeseries data.frame |
| `resample_to_field` | match GLM water temperatures with field observations |
| `run_analyzer` | runs Lake Analyzer on remote server for GLM outputs |
| `set_lke` | sets values in lke object |
| `set_nml` | sets values in nml object |
| `write_bth` | write Lake Analyzer *.bth file from GLM simulation |
| `write_lke` | write Lake Analyzer *.lke file from GLM simulation |
| `write_lvl` | write Lake Analyzer *.lvl file from GLM simulation |
| `write_nml` | write GLM *.nml for a GLM simulation |
| `write_wnd` | write Lake Analyzer *.wnd file from GLM simulation |
| `write_wtr` | write Lake Analyzer *.wtr file from GLM simulation |

FAQ
=====

How do I install `glmtools`? It isn't on CRAN!
===
`glmtools` can be installed from the [github.gleon.io](http://github.gleon.io "GLEON's github page") repository. To install `glmtools` and all dependencies, follow the appropriate instructions below:

For **windows**
use the following command in R:
    
    install.packages("ncdf4",
        repos = "http://gleon.github.com/",
        type = "win.binary")

then
    
    install.packages("glmtools", 
        repos = c("http://gleon.github.com/", "http://cran.us.r-project.org"),
        dependencies = TRUE,
        type = "both",
        INSTALL_opts = "--no-multiarch")

For **OSX** or **Linux**
use the following command in R:

    install.packages("glmtools", 
        repos = c("http://gleon.github.com/", "http://cran.us.r-project.org"),
        dependencies = TRUE,
        type = "both")
        
What libraries does `glmtools` need?
===
This version requires the NetCDF version 4 R library (called `ncdf4`), and `rLakeAnalyzer`, `RCurl`, and `tools` if you would like to run all examples. 
On Windows, `ncdf4` is not included on CRAN (as of right now, 2014-08-19). We have included a separately distributed version
on the [github.gleon.io](http://github.gleon.io "GLEON's github page") page, which will be used with the install directions outlined above. Currently, our support for `ncdf4` on windows is limited to **R version 3.0+**, so if you are using a version of R below 3.0 on **windows**, you will get an installation error. We will continue to update this in the future, but if you have problems, see the [ncdf4 project page](http://cirrus.ucsd.edu/~pierce/ncdf/ "ncdf4 project page"). 

How do I contribute new code back to the glmtools project?
===

In order to contribute to this code, we recommend the following workflow: 

1) "fork" this repository to your own personal github account

2) clone the github repository to your computer:

	$git clone https://github.com/{username}/glmtools.git

3) modify code or add new functionality, save the code

4) add the repository master to a remote master called "upstream"

	$cd glmtools

	$git remote add upstream https://github.com/GLEON/glmtools.git

5) before pushing your changes to your repository, pull in the current version of the GLEON master:

	$git fetch upstream

6) merge these differences with your own "master" version:

	$git merge upstream/master

7) push your changes to your github repository, in addition to changes made by pulling in the GLEON master:

	$git push

8) submit a pull request to GLEON master using your account at github.com

