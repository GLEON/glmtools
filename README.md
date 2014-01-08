rGLM
=====

Tools for interacting with the General Lake Model (GLM) in R. Includes some basic functions for calculating physical derivatives and thermal properties of model output.

FAQ
=====

How do I install rGLM? It isn't on CRAN!
===

It is easiest to install rGLM using a tool distributed in the CRAN package 'devtools'. Before you install rGLM,
you must first install the netcdf R library (see "How do I install ncdf4?" below).

Once you have ncdf4 installed. Install and then source the devtools package.

    install.packages('devtools')
    library(devtools)

Then, you can use the helper function to install rGLM from Github

    install_github("rGLM", "GLEON", args="--no-multiarch")

Note: The --no-multiarch is there because of a quirk of the ncdf4 library.

What libraries do I need?
===

This version requires the NetCDF version 4 R library (called ncdf4).

How do I install ncdf4? 
===

On Mac or Linux: Simply type 

    install.packages('ncdf4')

On Windows: This is trickier. CRAN does not contain a Windows binary install for 
the ncdf4 library (as of right now, 2013-05-02). I have included a separately distributed version
in the "lib" subdirectory. To install, type

    install.packages('%path%/lib/%arch%/ncdf4_1.9.zip')

%path% is the location of the rGLM directory.  
%arch% is the architecture (32 or 64 bit) you use to run R. You probably use x64.


How do I contribute new code back to the rGLM project?
===

In order to contribute to this code, we recommend the following workflow: 

1) "fork" this repository to your own personal github account

2) clone the github repository to your computer:

	$git clone https://github.com/username/rGLM.git

3) modify code or add new functionality, save the code

4) add the repository master to a remote master called "upstream"

	$cd rGLM

	$git remote add upstream https://github.com/GLEON/rGLM.git

5) before pushing your changes to your repository, pull in the current version of the GLEON master:

	$git fetch upstream

6) merge these differences with your own "master" version:

	$git merge upstream/master

7) push your changes to your github repository, in addition to changes made by pulling in the GLEON master:

	$git push

8) submit a pull request to GLEON master using your account at github.com

