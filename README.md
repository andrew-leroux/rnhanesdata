# rnhanesdata

You can install `rnhanesdata` from Github using the devtools package by running the following code:

```{r}
#install.package("devtools")
devtools::install_github("andrew-leroux/rnhanesdata")
```

If you want to build the package vignettes (recommended), you'll want to run the following code instead

```{r}
#install.package("devtools")
devtools::install_github("andrew-leroux/rnhanesdata", build = TRUE, 
                         build_opts = c("--no-resave-data", "--no-manual"))
```

Once this package is installed, see the help file associated with the package (?rnhanesdata) for an overview of the primary package
functions, as well as a high level description of the processed NHANES data included in the package. Each set of processed data includes it's own set of documentation.
