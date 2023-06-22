# Welcome!

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/seabbs/cste-forecasting-workshop/main?urlpath=rstudio)

This tutorial is meant to be an approachable introduction to estimating $R_t$.
It's meant for any and all levels of comfort with writing code.

This tutorial will work best if you spend a little time in advance to get set up.
It can take a little time for your computer to install the necessary packages (about 15 minutes).
If you've never used R before, follow the detailed instructions below for a
step-by-step guide to get set up.
If you have R and RStudio installed on your computer, feel free to skip ahead
[to the tutorial](https://samabbott.co.uk/cste-forecasting-workshop/tutorial.html).

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
run the code in `tutorial.Rmd` as part of the workshop.

If you have any issues, we also have a [binder](https://mybinder.org/v2/gh/seabbs/cste-forecasting-workshop/main?urlpath=rstudio) set up with all the necessary packages installed. This will take a little while to load but should allow you to run the code in `tutorial.Rmd` as part of the workshop. Note that this will not save any changes you make to the code and may be slower than running the code on your own machine. If you run into an SSL error you may need to change the link to `http` in the URL bar and allow change your browser settings to allow unsafe sites.

# Other resources

If you are interested in finding additional resources for estimating the effective reproduction number or learning about nowcasting in R, explore the following:

**`EpiNow2` resources**

- [Documentation](https://epiforecasts.io/EpiNow2/dev): The documentation for the `EpiNow2` package. This is the package we will be using in this tutorial. It is designed to be easy to use, robust to a wide range of contexts, and flexible.
- [CDC Mpox Technical reports](https://www.cdc.gov/poxvirus/mpox/cases-data/technical-report.html): For a recent example of `EpiNow2` in use, see the CDC's technical reports on Mpox. These reports use `EpiNow2` to estimate the effective reproduction number and forecast future cases.
- [Reflections on two years estimating the effective reproduction number](https://epiforecasts.io/posts/2022-03-25-rt-reflections/index.html): This blog post reflects on the development of `EpiNow2` and the challenges of estimating the effective reproduction number in real-time at scale.
- [Nowcasting example](https://github.com/epiforecasts/nowcasting.example): This is a repository that uses simulated data to demonstrate how to nowcast using both `EpiNow2` and `Epinowcast`.
- [Tutorial Q and A](https://github.com/seabbs/cste-forecasting-workshop/discussions/categories/q-a): If you have any questions about the tutorial, please post them here. We will try to answer them as quickly as possible.

**Other packages**

- [epinowcast](https://package.epinowcast.org): This package has been designed as the successor to `EpiNow2` and is currently under development. It is designed to be more general and even more flexible than `EpiNow2`.
- [epidemia](https://imperialcollegelondon.github.io/epidemia/index.html): This is another flexible package for estimating the effective reproduction number and forecasting. It is designed to be more flexible than `EpiNow2` and `epinowcast` but is potentially more difficult to use. It also generally has less functionality for dealing with delays than `EpiNow2` and `epinowcast`.
- [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html): This is a more mature package for estimating the effective reproduction number. It exploits a mathematically relationship to fit the renewal equation very quickly but is not currently able to handle reporting delays or to produce forecasts which the use of supporting packages.

**Papers**

- [Practical considerations for measuring the effective reproductive number, Rt](10.1371/journal.pcbi.1008409): This paper provides an overview of the renewal equation approach to estimating the effective reproduction number and discusses some of the practical considerations that need to be taken into account when using this approach.
- [Why are different estimates of the effective reproductive number so different? A case study on COVID-19 in Germany](https://doi.org/10.1101/2023.04.27.23289109): This paper explores the differences between different approaches to estimating the effective reproduction number and discusses the implications of these differences.
- [Commentary on the use of the reproduction number R during the COVID-19 pandemic](https://doi.org/10.1177/09622802211037079): This paper provides a commentary on the use of the reproduction number during the COVID-19 pandemic and discusses some of the challenges of estimating the reproduction number in real-time.