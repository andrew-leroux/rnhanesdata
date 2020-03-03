# rnhanesdata

You can install `rnhanesdata` from Github using the devtools package by running the following code:

```{r}
#install.package("devtools")
devtools::install_github("andrew-leroux/rnhanesdata")
```
You can view package vignettes at https://andrew-leroux.github.io/rnhanesdata/ under the "Articles" tab. Alternatively, if you want to build the package vignettes, you'll need to install some additional packages and modify the call to devtools::install_github(). The code to do so is below

```{r}
## install required CRAN packages
pckgs <- c("knitr","kableExtra",              ## packages used for creating Tables
           "devtools",                        ## package used to download R packages stored on GitHub
           "ggplot2", "gridExtra",            ## packages for plotting 
           "corrplot",                        ## for correlation plot
           "reshape2",                        ## for transforming data long -> wide and wide -> long 
           "dplyr",                           ## packages for merging/transforming data
           "survey",                          ## package used for analyzing complex survey data in R
           "mgcv", "refund"                   ## packages used for smoothing/fpca
           )
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
    install.packages(x)
    require(x, character.only=TRUE)
})
rm(list=c("pckgs"))
devtools::install_github("andrew-leroux/rnhanesdata", build_vignettes = TRUE, 
                         build_opts = c("--no-resave-data", "--no-manual"))
```

Once this package is installed, see the help file associated with the package (?rnhanesdata) for an overview of the primary package
functions, as well as a high level description of the processed NHANES data included in the package. Each set of processed data includes it's own set of documentation.
