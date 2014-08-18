glmtools
=====

Tools for interacting with the General Lake Model (GLM) in R. `glmtools` includes some basic functions for calculating physical derivatives and thermal properties of model output, and some plotting functionality (see example image below).

![alt tag](http://github.gleon.io/images/test_figure.png)

FAQ
=====

How do I install `glmtools`? It isn't on CRAN!
===

Before you install `glmtools`,
you must first install the other required libraries, including the netcdf R library (see "How do I install `ncdf4`?" below).

Once you have the other libraries installed, install glmtools from the stable release on gleon.github.io:

    install.packages("glmtools", 
        repos="http://gleon.github.com/", type="source")

What libraries do I need?
===
This version requires the NetCDF version 4 R library (called `ncdf4`), and you may want to install `rLakeAnalyzer`, `RCurl`, and `tools` if you could like to run all examples. Aside from `ncdf4`, all other libraries are on CRAN and can be installed with `install.packages()`.

How do I install `ncdf4`? 
===

On Mac or Linux: Simply type 

    install.packages('ncdf4')

On Windows: This is trickier. CRAN does not contain a Windows binary install for 
the ncdf4 library (as of right now, 2013-05-02). I have included a separately distributed version
in the "lib" subdirectory. To install, type

    install.packages('%path%/lib/x64/ncdf4_1.12.zip') # for 64 bit
OR

    install.packages('%path%/lib/i386/ncdf4_1.9.zip') # for 32 bit

%path% is the location of the glmtools directory.  
%arch% is the architecture (32 or 64 bit) you use to run R. You probably use x64.


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

