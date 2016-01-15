# VCD2R - Reading Value Change Dump Files into R

## Huh?

This is (hopefully) going to be a neat little package to read uncompressed VCD files into R for toggle counting, power estimation and simple side-channel analysis (on a teaching level).

It is also hands-on learning in writing a R package.

## Building the Package manually

clone the git repository

``` 
git clone https://github.com/wamserma/VCD2R.git 
```

install the build dependencies

```{r}
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
```

open project in RStudio and hit `Ctrl + Shift + L`
