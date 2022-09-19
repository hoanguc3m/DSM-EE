# DSM-EE

## Install R packages and dependences

``` r
install.packages(c("Rcpp", "midasml", "VineCopula", "matrixcalc", "HelpersMG", "moments", "plyr", "MASS", "future.apply"))
install.packages("DSCopula_0.1.0_R_x86_64-pc-linux-gnu.tar.gz", repos = NULL, type = "source")
```

## Load data
``` r
library("DSCopula")
library(lubridate)

load(file='alldf2.Rdata')


time_start = as.Date("1992-01-04")
time_end = as.Date("2021-12-30")

# If you can not wait for a couple of hours and run out of memory
time_end = as.Date("2000-12-30")

# Using 16 cores CPU
plan(list(tweak(multisession, workers = 16),
          tweak(multisession, workers = 1),
          tweak(multisession, workers = 1) ))

# Estimate the DSC model with copula famity from 1 to 6 (1: Gaussian, 2: Student, 3: Clayton, 4: Gumbel, 5: Frank, 6: Joe)

list_DSCopula <- list()
for (copula_fam in c(1:6)){
    List_input <- DS_data(data.u = cbind(bigdf$u_sp500, bigdf$u_wti),
                          data.udate = bigdf$date,
                          family = copula_fam,
                          est.start = time_start + 1, est.end = time_end - 1)
    data = List_input$data
    priors = List_input$priors
    arg = List_input$arg
    list_DSCopula[[copula_fam]] <-  fit.DSCopula(data, priors, arg)
    print(paste(" Copula ", copula_fam, " log ML = ", list_DSCopula[[copula_fam]]$log_llh ))
}

# Estimate the DSM model using RCor as a explanatory variable with copula famity from 1 to 6 (1: Gaussian, 2: Student, 3: Clayton, 4: Gumbel, 5: Frank, 6: Joe)

polynomial = "rbeta_w"
iK = 1; x.lag = 24;

{
    list_DCCMIDASCopula <- list()
    data.x = matrix(c(reduce_monthdata(bigdf$Rcor, bigdf$date)$data.x), ncol = 1)
    # Back to orginal month
    data.xdate = c(reduce_monthdata(bigdf$Rcor, bigdf$date)$data.xdate)

    List_input <- DSMIDAS_data(data.u = cbind(bigdf$u_sp500, bigdf$u_wti),
                               data.udate = bigdf$date,
                               data.x = data.x, data.xdate = data.xdate,
                               x.lag = x.lag, polynomial = polynomial,
                               family = 1,
                               est.start = time_start + 1, est.end = time_end - 1)
    data = List_input$data
    priors = List_input$priors
    arg = List_input$arg
    list_DCCMIDASCopula[[1]] <-  fit.DCCMIDASC(data, priors, arg)
    print(paste(" Copula ", 1, " log ML = ", list_DCCMIDASCopula[[1]]$log_llh ))
}


```
