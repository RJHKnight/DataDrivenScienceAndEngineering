# DataDrivenScienceAndEngineering

The excellent textbook Data Driven Science and Engineering (Machine Learning, Dynamical Systems, and Control) contains a large number of examples in Matlab and Python.

As I work my way through the book, I will attempt to port as many as possible of the examples to R.

## Installation and Running

All of the main scripts are self contained, the examples are arranged by Chapter the folders `Chapter_x`.

It is recommended to use the RStudio IDE - the whole project can be launched by running `DataDrivenScienceAndEngineering.Rproj`

To set up the required libraries, source the install script using:

```
source("Misc/Install.R")
```

### Some points of note on the libraries used

The Deep Neural Network examples in Chapter 6 use the keras framework. You can install the `keras` library from CRAN as normal but further steps are required to install the Python environment that is used behind the scenes. For most vanilla setups `keras::install_keras()` will do this all for you. If you run into any issues there is a lot of helpful information available at https://keras.rstudio.com/.

Some of the later control examples (Chapter 9/10) rely on non-trivial numerical algorithms for solutions to the Riccati and Lyapunov equations. The Matlab and Python examples use the Slicot library for this. It appears that a formal interface to SLICOT does not seem to exist for R, so I wrote one - hosted at https://github.com/RJHKnight/slicotr. It is still largely untested, but appears to work sufficiently well for the examples included here.

This package will require compilation of Fortan code. 

- For installation on Windows, ensure RTools is installed (https://cran.r-project.org/bin/windows/Rtools/)
- For installation on macOS (including M1), install Apple Xcode 10.1 and gFortran from https://github.com/fxcoudert/gfortran-for-macOS/releases


## References

* The main website for the book http://databookuw.com/
* The pdf of the book content http://databookuw.com/databook.pdf
* Steve Brunton's YouTube channel, with excellent lectures for chapters 1-3 & 7-10  https://www.youtube.com/c/Eigensteve
* Nathan Kutz's YouTube channel, with excellent lectures for chapters 4-6 & 11-12 https://www.youtube.com/c/NathanKutzAMATH
