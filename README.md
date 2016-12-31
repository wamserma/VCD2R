[![Build Status](https://img.shields.io/travis/wamserma/VCD2R/master.png)](https://travis-ci.org/wamserma/VCD2R/) [![Windows Build Status](https://img.shields.io/appveyor/ci/wamserma/VCD2R/master.svg)](https://ci.appveyor.com/project/wamserma/VCD2R) [![Coverage Status](https://img.shields.io/codecov/c/github/wamserma/VCD2R/master.svg)](https://codecov.io/github/wamserma/VCD2R?branch=master)

# VCD2R - Reading Value Change Dump Files into R

## Huh?

This is (hopefully) going to be a neat little package to read uncompressed VCD files into R for toggle counting, power estimation and simple side-channel analysis (on a teaching level).

## Install from GitHub

```r
install.packages(c("devtools")) # if you haven't done that before
devtools::install_github("wamserma/VCD2R",dependencies=TRUE,upgrade_dependencies=TRUE)
```

## Building the Package manually

clone the git repository

```bash 
git clone https://github.com/wamserma/VCD2R.git 
```

install the build dependencies

```r
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "covr"))
```

open project in RStudio and hit `Ctrl + Shift + L`

## converting VCD+ to VCD

If you have a ```.vpd``` file e.g. from Synopsys VCS, you can convert it to an (uncompressed) VCD file by calling

```bash
vcs -vpd2vcd inter.vpd inter.vcd
```

if you want to capture version information, you might want to use

```bash
echo '$version' > inter.vcd
vcs -vpd2vcd | head -5 >> inter.vcd
echo '$end' >> inter.vcd
echo '$date' $'\n''dumped:' $(date -r inter.vpd -R) $'\n''converted:' $(date -R) $'\n''$end' >> inter.vcd
vcs -vpd2vcd inter.vpd -q >> inter.vcd
```



## parsers in other languages

I made this because I wanted a stream oriented parser to make toggle count statistics in R. 
As far as I know, there is no other [R] VCD parser. Below is a list with parsers in other languages, without making any assurance on functionality or performance (some are just for parsing, some can also do toggle counts).

### Haskell
[VCD](https://github.com/tomahawkins/vcd/) on [Hackage](https://hackage.haskell.org/package/vcd-0.2.2/)

### Python
[verilog_VCD](https://pypi.python.org/pypi/Verilog_VCD)  
[vcd_parser](https://github.com/GordonMcGregor/vcd_parser)

### Java
[VCDFileParser](http://www.edautils.com/VCDFileParser.html) (download needs [registering](http://form.jotformpro.com/form/40758626201957))

### C++
[cppVCD](https://github.com/wsong83/cppVCD)
[vcdparser](https://code.google.com/p/vcdparser/)
