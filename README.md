
<!--- README.md is generated from README.Rmd. Please edit that file -->

## whatdata: Import and export management action data

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/experimental)

### Overview

The *whatdata R* package provides functions for importing and exporting
management action data. It provides consistent data format standards for
the [What To Do](https://github/NCC-CNC/whattodo) and [What Template
Maker](https://github/NCC-CNC/whattemplatemaker) web applications. It is
not designed for general purpose use outside of these applications.

### Installation

The package is only available from an [online coding
repository](https://github.com/NCC-CNC/whatdata). To install it, use the
following R code.

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("NCC-CNC/whatdata")
```
