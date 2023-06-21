# Welcome!

This tutorial is meant to be an approachable introduction to estimating $R_t$.
It's meant for any and all levels of comfort with writing code.

This tutorial will work best if you spend a little time in advance to get set up.
It can take a little time for your computer to install the necessary packages (about 15 minutes).
If you've never used R before, follow the detailed instructions below for a
step-by-step guide to get set up.
If you have R and RStudio installed on your computer, feel free to skip ahead
[to the tutorial](https://samabbott.co.uk/cste-forecasting-workshop/epinow2.html) epinow2.md.

# Getting set up

The set up process comes in three parts:

1. Downloading the folder with the necessary code onto your machine. If this
document is sitting in your computer in a folder called `cste-forecasting-workshop`,
then you've successfully completed this step. If not, look for a file with a *.zip
extension in an email. Download and unzip the folder.
  - If you're comfortable with Git and Github, you can clone the repository
  from `seabbs/cste-forecasting-workshop`

2. Download and install R and Rstudio [from this website](https://posit.co/download/rstudio-desktop/)

3. Double click on the `cste-forecasting-workshop.Rproj` object in the 
`cste-forecasting-workshop` folder to open the R project associated with this
tutorial. This should launch R and RStudio. Then, type 
`renv::restore(prompt = FALSE)` into the open console (with the `>` symbol) to
download all the necessary packages.

These steps should take around 20 minutes. Once completed, you're all set up to
run the code in `epinow2.Rmd` as part of the workshop.

# Other resources

If you're looking for more resources on estimating the effective reproduction number, or nowcasting, check out the following:

- [EpiNow2 website](https://epiforecasts.io/EpiNow2/dev): The documentation for the `EpiNow2` package. This is the package we will be using in this tutorial. It is designed to be easy to use, robust to a wide range of contexts, and flexible.
- [epinowcast](https://package.epinowcast.org): This package has been designed as the successor to `EpiNow2` and is currently under development. It is designed to be more generalised flexible than `EpiNow2`.
- [epidemia](https://imperialcollegelondon.github.io/epidemia/index.html): This is another flexible package for estimating the effective reproduction number and forecasting. It is designed to be more flexible than `EpiNow2` and `epinowcast` but is potentially more difficult to use. It also generally has less functionality for dealing with delays than `EpiNow2` and `epinowcast`.
- [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html): This is a more mature package for estimating the effective reproduction number. It exploits a mathematically relationship to fit the renewal equation very quickly but is not currently able to handle reporting delays or to produce forecasts which the use of supporting packages

