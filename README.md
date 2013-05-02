GLM-r
=====

Tools for interacting with the General Lake Model (GLM) in R. 

FAQ
=====

===What libraries do I need?

This version requires the NetCDF version 4 R library (called ncdf4).

===How do I install ncdf4? 

On Mac or Linux: Simply type 

    install.packages('ncdf4')

On Windows: This is trickier. CRAN does not contain a Windows binary install for 
the ncdf4 library (as of right now, 2013-05-02). I have included a separately distributed version
in the "lib" subdirectory that works on the 32-bit version of Windows R (I think v2.15-ish). To install, 
type

    install.packages('%path%/lib/ncdf4_1.4.zip')

where %path% is the location of the GLM-r directory.
