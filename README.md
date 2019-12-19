# ezmmek

<!-- badges: start -->
<!-- badges: end -->

ezmmek provides a suite of functions that calibrate, calculate, and plot enzyme activities as they relate to the degradation of synthetic substrates. It generates plots and statistics regarding both standard curves and saturation curves. Plots are customizable through a series of prompts.

***

## Installation

You can install the released version of ezmmek from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ezmmek")
```
***
You can install the released version of ezmmek from GitHub (https://github.com) with :

```r
install_github("ccook/ezmmek")
```
***

## Packages needed
    assertable  
    dplyr  
    ggplot2  
    magrittr  
    nls2  
    purrr  
    scales  
    tidyr

***

## Examples
Create standard curve and generate linear model summary
``` r
p_std_curve(d_std)
```    
***
Create plot of raw saturation data
```r
p_sat_raw(d_sat)
```
***
Create saturation curve and generate Km and Vmax values
```r
p_sat_curve(d_std, d_sat)
```
***
Create saturation curve with normalization factor and generate Km and Vmax values
```r
p_sat_curve(d_std, d_sat_n)
```
***
Add additional argument for plot customization
```r
p_std_curve(d_std, man.units = TRUE)
```

```r
p_sat_raw(d_sat, man.units = TRUE)
```

```r
p_sat_curve(d_std, d_sat, man.units = TRUE)
```

```r
p_sat_curve(d_std, d_sat_n, man.units = TRUE)
```
***

## Authors
Christopher L. Cook (<ccook62@vols.utk.edu>) and Andrew D. Steen

***

## License
This project is licensed under AGPL-3.





