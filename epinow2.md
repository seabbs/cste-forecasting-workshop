---
title: Estimating reporting delays and nowcasting/forecasting infections with EpiNow2 using HHS COVID-19 hospitalizations
author: "Sam Abbott, Zachary Susswein, and Kaitlyn Gostic"
output:
  html_document:
    theme: cosmo
    toc: true
    toc_float: true
    toc_depth: 4
    includes:
      before_body: html/header.html
      after_body: html/footer.html
    self_contained: true
    keep_md: true
date: "June 20, 2023"
---



_This tutorial is partly based on this [nowcasting example](https://github.com/epiforecasts/nowcasting.example) from Sam Abbott and Sebastian Funk._

## Summary

- In this tutorial we make use of [EpiNow2](https://epiforecasts.io/EpiNow2/), a toolset for real-time analysis of infectious disease dynamics, for estimating reporting delays and using them to conduct a nowcast. 
- We first explore the HSS hospitalization data and estimate the reporting delay distribution using `EpiNow2`.
- We then draw from the literature to define a plausible incubation period and reporting delay
- We then show how the these estimates may be used in `EpiNow2` to perform nowcasts, forecasts and estimation of reproduction numbers and growth rates. 
- Finally, we summarize strengths and weaknesses of this approach and highlight other options and areas for future work.

For more about the `EpiNow2` package see the [the package documentation](https://epiforecasts.io/EpiNow2/). An [alternative approach](https://package.epinowcast.org) to the problems discussed in this tutorial is contained in the `epinowcast` package. This package is still under active development but aims to address the limitations `EpiNow2` with a view to eventually replacing it.

## Load required libraries

We first load the packages required for this tutorial. These can be installed or staged

```r
renv::restore()
```

<<<<<<< HEAD
> :warning: **`renv::restore()` will take some time to run and produce output**: It is a command that will automatically load or install the correct versions of all packages needed to run this analysis. Ideally we recommend doing this sometime before you want to work through the rest of the tutorial as it may take some time.
=======
> :warning: **`The first run of renv::restore()` will take some time**: It is a command that will automatically install the correct versions of all packages needed to run this analysis. Ideally, we recommend running it sometime before you want to work through the rest of the tutorial. Once you have all the necessary packages installed, subsequent calls to `renv::restore()` should be quick. 

>>>>>>> 0785fb6 (KMG copy edits)


```r
# Note this is the development version of EpiNow2.
# Docs here: https://epiforecasts.io/EpiNow2/dev/
library("EpiNow2")
library("dplyr") # for manipulating data
library("ggplot2") # for plotting data
library("tidyr") # for manipulating data
library("purrr") # for manipulating data
library("covidcast") # for downlooading HHC hospitalization data
library(here) # for file paths
```


## Load the data

We use the [covidcast](https://cmu-delphi.github.io/covidcast/covidcastR/) package to read in [HHS data](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/hhs.html) on confirmed Covid-19 hospital admissions. 
The primary source for these data is the HHS state-level COVID-19 hospitalization time series, which can be found on [HealthData.gov](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh).
The [covidcast](https://cmu-delphi.github.io/covidcast/covidcastR/) package provides a convenient way to read in a subset of the full data set.
Here, we focus on the data from New York, Utah, Ohio, Virginia, and North Carolina during the Omicron wave in the winter of 2021-2022. 


We _could_ download the most recent version of this data directly using:


```r
covid_hospitalizations <- covidcast_signal(
      data_source = "hhs", 
      signal = "confirmed_admissions_covid_1d",
      start_day = "2021-12-01",
      end_day = "2022-02-01",
      geo_type = "state",
      geo_value = c("ny", "ut", "oh", "va", "nc")
    ) |>
      as_tibble() |>
      filter(issue == max(issue)) |>
      select(geo_value, time_value, value) |>
      rename(state = geo_value, date = time_value, confirm = value)
```

<<<<<<< HEAD
Rather than just downloading the data as it is available now we can also download versions of the data that would have been available at the time. This is equivalent to having a linelist with columns for the date of hospitalisiation and the date of report (*To download this much data you may need to register for a [`covidcast` API key](https://cmu-delphi.github.io/covidcast/covidcastR/reference/covidcast_signal.html). This then needs to be loaded into R using `options(covidcast.auth = "<your-api-key")`*).
=======
The above command pulls the most up-to-date version of the data and stores it in a data frame called `covid_hospitalizations`.


But when evaluating methods, we're often interested in how they would have performed
on the data available at the time rather than the data in its final, fully-reported state.
This data format is equivalent to having a linelist with columns for the date of hospitalization and the date of report.

If you register for a [`covidcast` API key](https://cmu-delphi.github.io/covidcast/covidcastR/reference/covidcast_signal.html),
you can run the below command to download the data as of a specific report date. (You **don't need to run this command yourself**! Go to the next step.)

> :bulb: We use the terms **report date** and **vintage** to indicate the as-of date (on what date was this snapshot of the data observed). We use **event date** or simply **date** to indicate where a data point falls along the x-axis of the epidemic time series. 

>>>>>>> 0785fb6 (KMG copy edits)


```r
# Define a range of as-of dates
as_of_dates <- seq(as.Date("2021-12-03"), as.Date("2022-03-01"), by = "day")

# Read in the data for each as-of date
covid_hospitalizations_by_vintage <- as_of_dates |>
  purrr::map_df(\(x) (
    covidcast_signal(
      data_source = "hhs",
      signal = "confirmed_admissions_covid_1d",
      start_day = "2021-12-01",
      end_day = "2022-02-01",
      as_of = x,
      geo_type = "state",
      geo_value = c("ny", "ut", "oh", "va", "nc")
    ) |>
      mutate(report_date = x) |> # Add the date of report
      select(report_date, geo_value, time_value, value) |> # keep these columns
      rename(state = geo_value, # rename the columns for readability
             date = time_value, 
             confirm = value) |>
      arrange(date, report_date) 
  )) |>
  as_tibble()
```


Rather make everyone get an API key and query the `covidcast` API, we instead use the data we have already downloaded.


```r
# Read in the data, which are saved as a .rds file in the project folder
covid_hospitalizations <- readRDS(
  here("data", "covid_hospitalizations.rds")
) |>
  as_tibble()

glimpse(covid_hospitalizations)
```

```
## Rows: 315
## Columns: 3
## $ state   <chr> "nc", "ny", "oh", "ut", "va", "nc", "ny", "oh", "ut", "va", "n…
## $ date    <date> 2021-12-01, 2021-12-01, 2021-12-01, 2021-12-01, 2021-12-01, 2…
## $ confirm <dbl> 128, 443, 576, 78, 113, 169, 490, 620, 82, 137, 157, 444, 571,…
```

```r
covid_hospitalizations_by_vintage <- readRDS(
  here("data", "covid_hospitalizations_by_vintage.rds")
) |>
  as_tibble()

glimpse(covid_hospitalizations_by_vintage)
```

```
## Rows: 18,265
## Columns: 4
## $ report_date <date> 2021-12-03, 2021-12-03, 2021-12-03, 2021-12-03, 2021-12-0…
## $ state       <chr> "nc", "ny", "oh", "ut", "va", "nc", "ny", "oh", "ut", "va"…
## $ date        <date> 2021-12-01, 2021-12-01, 2021-12-01, 2021-12-01, 2021-12-0…
## $ confirm     <dbl> 140, 456, 574, 80, 107, 141, 439, 556, 79, 114, 141, 435, …
```

# Data exploration

## Visualize hospitalizations

We start by visualizing the currently reported hospitalizations for this time period.
We're treating these reports as finalized because its unlikely there will be
substantial revisions in hospitalizations from so long ago.


```r
covid_hospitalizations |>
  ggplot() +
  aes(x = date, y = confirm) +
  geom_col(alpha = 0.6) +
  theme_bw() +
  labs(
    x = "date of hospitalization",
    y = "incident hospitalizations",
    title = "hospitalizations in select sates for the winter 2021-2022 wave"
  ) +
  facet_wrap(vars(state), ncol = 1, scales = "free_y")
```

![](epinow2_files/figure-html/plot-obs-1.png)<!-- -->

<<<<<<< HEAD
As expected we see a large increase in hospitalisations in all states during the winter 2021-2022 wave. However, we can also see that there is a large amount of variation in the number of hospitalisations reported on each date and state by state. 

## Visualise hosptialisations by date of report

Whilst it is interesting to look back retrospectively when analysing data it is important to remember that the data we have now is not the same as the data that was available at the time. This can occurr for a number of reasons, for example, there may be a delay between the date of hospitalisation and the date of report or there may be changes in how hospitalisations are measured which leads to a retrospective change in the number of hospitalisations.

To begin to unpick this we can look at the number of hospitalisations as it was reported. We should see that that the number of hospitalisations reported on each date increases over time as more data becomes available if the changes are caused only by delays. If reporting is more complex than this, for example, if hospitalisations can be recategorised to a different day then the number of hospitalisations reported for a given day may decrease as well as increase over time.
=======
## Visualize hospitalizations by date of report

We can compare how hospitalization reporting changed over time by overlaying onto
the plot what was actually observed at the time. 
We can do this by using the `covid_hospitalizations_by_vintage` data frame we
generated above to find the number of hospitalizations that were reported on each date.
>>>>>>> 0785fb6 (KMG copy edits)


```r
covid_hospitalizations |>
  ggplot() +
  aes(x = date, y = confirm) +
  geom_point(alpha = 0.6) +
  geom_line(
    data = covid_hospitalizations_by_vintage,
    aes(x = date, y = confirm, col = report_date, group = report_date),
    alpha = 0.8
  ) +
  scale_y_log10() +
  theme_bw() +
  labs(
    x = "Date of hospitalization",
    y = "Hospitalizations",
    title = "Hospitalizations in select states for the winter 2021-2022 wave"
  ) +
  guides(col = guide_colorbar(title = "Date of report", barwidth = 15)) +
  theme(legend.position = "bottom") +
  facet_wrap(vars(state), ncol = 1, scales = "free_y")
```

![](epinow2_files/figure-html/plot-obs-vintage-1.png)<!-- -->

<<<<<<< HEAD
You should be able to see here that the data as observed in real-time is right truncated due to delays in reporting during the exponential phase of the wave (i.e., it is an undercount of what will eventually be reported). If left uncorrected this can lead to an underestimation of the effective reproduction number, inaccurate forecasts, and potentially mislead policy makers using these metrics.

However, it appears that hospitalisations are also very commonly corrected down, this indicates a more complex reporting mechanism is at play. Unfortunately, currently currently there are few available methods that can address this kind of reporting structure and the development of new ones is likely dependent on the interaction between those collecting the data and those developing new methods.
=======
You should be able to see here that the data as observed in real time is right truncated, meaning that observations at the end of the time series are incomplete or subject to revision.
(The light blue line and points represent final counts. The short darker lines represent preliminary counts that will later be revised.)
In most epidemiological data, delays in reporting lead to a temporary undercount of what will eventually be reported. 
If left uncorrected when using real-time data sets, this truncation can lead to underestimation of the effective reproduction number, inaccurate forecasts, and potentially mislead reports policy makers using these metrics.
>>>>>>> 0785fb6 (KMG copy edits)

## Visualize the reporting delay

<<<<<<< HEAD
Another way at looking at this is to plot the distribution of reporting delays. We can do this by calculating the delay between the date of hospitalisation and the date of report for each hospitalisation.
=======
We can also plot the distribution of reporting delays to look at how much delay is normal. 
For this plot, we calculate the delay between the date of hospitalization and the date of report for each hospitalization.
>>>>>>> 0785fb6 (KMG copy edits)


```r
covid_hospitalizations_reporting_cdf <-
  covid_hospitalizations_by_vintage |>
  filter(date >= as.Date("2021-12-01")) |>
  group_by(date, state) |>
  group_modify(
    ~ mutate(.x,
      diff = confirm - lag(confirm, default = 0),
      final_reported = .x |>
        filter(report_date == max(report_date)) |>
        pull(confirm)
    )
  ) |>
  ungroup() |>
  mutate(
    delay = as.numeric(report_date - date),
    cdf = confirm / final_reported
  )

glimpse(covid_hospitalizations_reporting_cdf)
```

```
## Rows: 18,265
## Columns: 8
## $ date           <date> 2021-12-01, 2021-12-01, 2021-12-01, 2021-12-01, 2021-1…
## $ state          <chr> "nc", "nc", "nc", "nc", "nc", "nc", "nc", "nc", "nc", "…
## $ report_date    <date> 2021-12-03, 2021-12-04, 2021-12-05, 2021-12-06, 2021-1…
## $ confirm        <dbl> 140, 141, 142, 142, 128, 128, 128, 128, 128, 128, 128, …
## $ diff           <dbl> 140, 1, 1, 0, -14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ final_reported <dbl> 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, …
## $ delay          <dbl> 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
## $ cdf            <dbl> 1.093750, 1.101562, 1.109375, 1.109375, 1.000000, 1.000…
```

```r
covid_hospitalizations_reporting_cdf |>
  filter(delay <= 10) |>
  ggplot() +
  aes(x = delay, y = cdf, group = date) +
  geom_step(alpha = 0.4) +
  theme_bw() +
  labs(
    x = "Reporting delay (days)",
    y = "Hospitalizations reported relative to the final count",
    title = "Reporting delay in select states for the winter 2021-2022 wave"
  ) +
  facet_wrap(vars(state), ncol = 1, scales = "free_y")
```

![](epinow2_files/figure-html/plot-delay-1.png)<!-- -->

<<<<<<< HEAD
For the rest of this tutorial we focus on Ohio. You can repeat the analysis for other states by changing the `state` variable being `filter`ed for in the following code chunks.

## Estimating the reporting delay

We use `EpiNow2` to estimate the distribution of reporting delays for Ohio as this method can account for right truncation when estimating delays. This will then allow us to correct for right truncation in the data, if it is present, when we estimate the reproduction number. Unfortunately (as already noted), the current model cannot account for over reporting and so we will first have to remove this from the data.
=======
For the rest of this tutorial we focus on Ohio. You can repeat the analysis for other states by changing the `state` variable in the code below.

## Estimating the reporting delay

We use `EpiNow2` to estimate the distribution of reporting delays for Ohio. 
The model in `EpiNow2` allow us to correct for right truncation in the data when we estimate the reproduction number. 
Unfortunately, this model cannot account for over-reporting and so we will first have to remove this from the data.
>>>>>>> 0785fb6 (KMG copy edits)


```r
options(mc.cores = 4)

truncation_est <- covid_hospitalizations_reporting_cdf |>
  filter(state == "oh") |>
  filter(date >= as.Date("2021-12-14")) |>
  filter(report_date >= as.Date("2021-12-31")) |>
  filter(report_date <= as.Date("2022-01-14")) |>
  group_by(date) |>
  mutate(confirm = max(confirm, dplyr::lag(confirm, default = 0))) |>
  ungroup() |>
  # over the new year reporting was delayed. This leads to problems for the model
  filter(report_date != as.Date("2022-01-05")) |>
  select(report_date, date, confirm) |>
  group_split(report_date) |>
  map(~select(., -report_date)) |>
  estimate_truncation(
    trunc_max = 7,
    chains = 4, iter = 2000,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    verbose = FALSE
  )

## Make the output a dist_spec object
truncation_dist <- do.call(
  dist_spec, c(truncation_est$dist)
)

truncation_dist
```

```
## 
##   Uncertain lognormal distribution with (untruncated) logmean -1.2 (SD 0.64) and logSD 0.27 (SD 0.22)
```


```r
plot(truncation_dist)
```

![](epinow2_files/figure-html/plot-truncation-1.png)<!-- -->

**There is very little truncation in this data for this time period.** This is good news as it means we can use the data as observed to estimate the reproduction number..

## Generation time estimate

The generation time is the time between infection of an individual and infection of their infector.
In order to estimate the effective reproduction number with the renewal equation we need an estimate of the generation time.
With this quantity, we relate the number of infections on day $t$ to the number of infections on day $t - \tau$ where $\tau$ is the index of the generation time distribution. Mathematically this is

$$ I_t = R_t \sum_{tau = 1}^T I_{t - \tau} G(\tau), $$

<<<<<<< HEAD
where $T$ is the maximum lenght of the generation time. Rather than estimating this here instead we use an estimate from the literature. Specifically we use an estimate from Ganyani et al. (2020) which is based on several hundred cases of COVID-19 in China. For a real-world analysis we recommend thinking carefully about which generation time distribution to use. For example, the Ganyani et al. (2020) estimate is based on a sample of cases from China and so may not be representative of the generation time in other settings.
=======
where $T$ is the maximum length of the generation time. 
Rather than estimating the generation time here, we instead use an estimate from the literature. 
Specifically we use an estimate from Ganyani et al. (2020) which is based on 468 cases of COVID-19 in China.
For a real-world analysis we recommend thinking carefully about which generation time distribution to use. 
For example, the Ganyani et al. (2020) estimate is based on a sample of cases from China and so may not be representative of the generation time in other settings.
>>>>>>> 0785fb6 (KMG copy edits)


```r
generation_time <- get_generation_time(
  disease = "SARS-CoV-2", source = "ganyani",
  max = 10, fixed = TRUE
)
generation_time
```

```
## 
##   Fixed distribution with PMF [0.18 0.2 0.17 0.13 0.1 0.074 0.054 0.039 0.028 0.02]
```

We can now visualize this distribution.


```r
plot(generation_time)
```

![](epinow2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
The dark bars show a histogram of the generation time probabilities by day. The step plot shows the cumulative probability.

## Delays from infection to hospitalization

The delay between infection and hospitalization can be decomposed into two distributions: the incubation period and the delay from symptom onset to hospitalization. We can estimate these distributions using `EpiNow2` but for this tutorial we will use estimates from the literature.

### Incubation period

<<<<<<< HEAD
The incubation period is the time between infection and symptom onset. Here we use an estimate from Lauer et al. (2020) which is based again based on a few hundred cases of COVID-19 in China. For a real-world analysis we recommend thinking carefully about which incubation period distribution to use just as we did for the generation time.
=======
The incubation period is the time between infection and symptom onset. Here we use an estimate from Lauer et al. (2020) which is based on 181 cases of COVID-19 in China. For a real-world analysis we recommend thinking carefully about which incubation period distribution, just as we did for the generation time.
>>>>>>> 0785fb6 (KMG copy edits)


```r
incubation_period <- get_incubation_period(
  disease = "SARS-CoV-2", source = "lauer", fixed = TRUE,
  max = 15
)
incubation_period
```

```
## 
##   Fixed distribution with PMF [5.3e-05 0.013 0.093 0.18 0.2 0.17 0.12 0.083 0.053 0.033 0.02 0.012 0.0074 0.0046 0.0028]
```

We can now visualize this distribution.


```r
plot(incubation_period)
```

![](epinow2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Delay from symptom onset to hospitalization

<<<<<<< HEAD
The delay from symptom onset to hospitalisation is the time between symptom onset and hospital admission. This typically depends on the severity of sypmtoms, the robustness of the health system and the behaviour of the individual. Here we use a toy estimate motivated by our experience but ideally data would be available to estimate this quantity.
=======
The delay from symptom onset to hospitalization is the time between symptom onset and hospital admission. This time typically depends on the severity of symptoms, the robustness of the health system and the behavior of the individual. Here we use a toy estimate but ideally data would be available to estimate this quantity.
>>>>>>> 0785fb6 (KMG copy edits)


```r
## Delay from symptom onset to report
reporting_delay <- dist_spec(
  mean = convert_to_logmean(3, 1),
  sd = convert_to_logsd(3, 1),
  max = 10
)
reporting_delay
```

```
## 
##   Fixed distribution with PMF [0.00064 0.14 0.43 0.29 0.11 0.03 0.008 0.0021 0.00053 0.00014]
```

We can again visualize this distribution.


```r
plot(reporting_delay)
```

![](epinow2_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Convolving the delay from infection to hospitalization

<<<<<<< HEAD
As the incubation period and reporting delay can be represented as probability mass functiosn (i.e., vectors of probabilities) we can convolve them to find the distribution of delays from infection to hospitalisation. This helps reduce the computational burden of the model as we do not need to model multiple delays. It is also useful as it allows us to understand the combined effect of the incubation period and reporting delay on the delay from infection to hospitalisation.
=======
As the incubation period and reporting delay can be represented as probability mass functions (i.e., vectors of probabilities) we can convolve them to find the distribution of delays from infection to hospitalization.
(Convolution is the mathematical operation used to add random variables together. Essentially, we are adding the incubation and reporting delay distributions. Normally you wouldn't be able to add two distributions together with a plus sign, but EpiNow2 provides special functionality that lets us do this.)
Calculating this convolution outside of the model helps reduce the computational burden as we do not need to account for multiple delays.
>>>>>>> 0785fb6 (KMG copy edits)


```r
inf_to_hospitalization <- incubation_period + reporting_delay
```

We can now plot this convolved distribution.


```r
plot(inf_to_hospitalization)
```

![](epinow2_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

## Putting it all together into a nowcast

Now we have all the components we need to construct a nowcast. First we construct the data set of hospitalizations we wish to nowcast using data as available on the 14th of January 2022.


```r
oh_hosp_14th <- covid_hospitalizations_by_vintage |>
  filter(state == "oh") |>
  filter(report_date == as.Date("2022-01-14")) |>
  select(date, confirm)

glimpse(oh_hosp_14th)
```

```
## Rows: 43
## Columns: 2
## $ date    <date> 2021-12-01, 2021-12-02, 2021-12-03, 2021-12-04, 2021-12-05, 2…
## $ confirm <dbl> 576, 620, 571, 532, 562, 594, 680, 687, 667, 634, 615, 566, 60…
```

In order to evaluate our model we will use the most recently reported data up to the 28th of January 2022 (as we are forecasting for a week).


```r
oh_hosp_28th_retro <- covid_hospitalizations |>
  filter(state == "oh") |>
  filter(date <= as.Date("2022-01-28")) |>
  select(date, confirm)
```

<<<<<<< HEAD
We then use the `estimate_infections()` function contained in `EpiNow2` on this data set to obtain a nowcast, forecast and reproduction number estimate. This model uses a renewal equation based method to estimate the reproduction number and then convolves this with the incubation period and reporting delay we defined earlier to obtain a nowcast. It generates a forecast extrapolating the reproduction number estimate into the future. This is just an example model as there are many other ways to estimate the reproduction number and many other ways to extrapolate it into the future. We recommend thinking carefully about which model to use for your analysis.
=======
We then use the `estimate_infections()` function contained in `EpiNow2` on this data set to obtain a nowcast, forecast, and reproduction number estimate. Note: we haven't adjusted for truncation as there was little evidence of truncation in the weeks directly before this date.
>>>>>>> 0785fb6 (KMG copy edits)


```r
options(mc.cores = 4)

rt_estimates <- estimate_infections(
  reported_cases = oh_hosp_14th,
  # Our generation time estimate is first preprocessed into a format the model
  # understands
  generation_time = generation_time_opts(generation_time),
<<<<<<< HEAD
  # Similarly our delay from infection to hospitalisation is also preprocessed.
  delays = delay_opts(inf_to_hospitalisation),
=======
  delays = delay_opts(inf_to_hospitalization),
>>>>>>> 0785fb6 (KMG copy edits)
  rt = rt_opts(
    # Here we specify a prior for the initial value of the reproduction number
    # We set this to be near 1 as we expect the epidemic to be growing slowly
    # at the start of the period of interest
    prior = list(mean = 1, sd = 0.1),
    # This indicates the period of the random walk we wish to use (7 days).
    rw = 7
  ),
  # Here we have turned off the default Gaussian process prior in favor of 
  # the random walk specified in rt_opts.
  # This speeds up the code.
  gp = NULL,
<<<<<<< HEAD
  # These options control the MCMC sampler used to estimate the posterior
  # This uses the No-U-Turn sampler with a target acceptance rate of 99%
  # and 2000 samples with 500 warmup iterations.
=======
  # These options control the No U-Turn Sampler (NUTS) that we use to fit the 
  # model to data 
>>>>>>> 0785fb6 (KMG copy edits)
  stan = stan_opts(
    control = list(adapt_delta = 0.99),
    samples = 2000, warmup = 500
  ),
<<<<<<< HEAD
  # These options control the observation model. We use a Poisson observation
  # model with a day of the week effect
  obs = obs_opts(
    family = "poisson", week_effect = TRUE
  ),
  # This controls the forecast horizon. We forecast 14 days into the future.
=======
  # These options tell the model to use a Poisson error structure, and to 
  # adjust for day-of-week effects
  obs = obs_opts(
    family = "poisson", week_effect = TRUE
  ),
  # We set a 14 day forecast horizon
>>>>>>> 0785fb6 (KMG copy edits)
  horizon = 14
)
```

 Note we haven't adjusted for truncation as there was little evidence of truncation in the weeks directly before this date (as noted above).

## Visualising the results

### Effective reproduction number estimates

<<<<<<< HEAD
Using the output of `estimate_infections()` we can visualise the effective reproduction number estimates using a call to `plot()` (this has a range of other plotting options which can be explored using `?EpiNow2:::plot.estimate_infections`).
=======
Because we specified a weekly random walk for $R_t$, our estimates show weekly steps.
>>>>>>> 0785fb6 (KMG copy edits)


```r
plot(rt_estimates, type = "R")
```

![](epinow2_files/figure-html/plot_rt-1.png)<!-- -->

### Predicted hospitalizations

Another useful plot is the predicted hospitalisations. This can be obtained using the `plot_estimates()` function. This function takes a data frame of estimates and a data frame of reported cases and plots the estimates alongside the reported cases. 

Whilst the forecast is useful it is important to note that it is based on the assumption that the reproduction number remains constant at the last estimated value (though other options are supported in the package). This is unlikely to always be true in practice and so the forecast should be communicated with this in mind. 


```r
rt_estimates |>
  pluck("summarised") |>
  filter(variable %in% "reported_cases") |>
  plot_estimates(
    reported = oh_hosp_28th_retro
  )
```

![](epinow2_files/figure-html/plot_hospitalizations-1.png)<!-- -->


We see that the forecast performs relatively well when compared to more recent data.

## Summary

- We have shown how to use `EpiNow2` to estimate the effective reproduction number and forecast hospitalisations.
- We have also explored how to use `EpiNow2` to estimate the delay from infection to hospitalisation.
- Finally, we have discussed some limitations of the approach and how to communicate these.

### Strengths

### Limitations

### Other resources

- [EpiNow2 website](https://epiforecasts.io/EpiNow2/)
- [epinowcast](https://package.epinowcast.org): This package has been designed as the successor to `EpiNow2` and is currently under development. It is designed to be more flexible and easier to use than `EpiNow2`.
- [epidemia](https://imperialcollegelondon.github.io/epidemia/index.html): This is another flexible package for estimating the effective reproduction number and forecasting. It is designed to be more flexible than `EpiNow2` and `epinowcast` but is potentially more difficult to use. It also generally has less functionality for dealing with delays than `EpiNow2` and `epinowcast`.
- [EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/index.html): This is a more mature package for estimating the effective reproduction number. It exploits a mathematically relationship to fit the renewal equation very quickly but is not currently able to handle reporting delays or to produce forecasts which the use of supporting packages

## References
