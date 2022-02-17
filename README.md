
<!--- README.md is generated from README.Rmd. Please edit that file -->

## whatdataio: Import and export management action data

[![lifecycle](https://img.shields.io/badge/Lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/experimental)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/workflow/status/NCC-CNC/whatdataio/Ubuntu/master.svg?label=Ubuntu)](https://github.com/NCC-CNC/whatdataio/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/workflow/status/NCC-CNC/whatdataio/Windows/master.svg?label=Windows)](https://github.com/NCC-CNC/whatdataio/actions)
[![R-CMD-check-Mac-OSX](https://img.shields.io/github/workflow/status/NCC-CNC/whatdataio/Mac%20OSX/master.svg?label=Mac%20OSX)](https://github.com/NCC-CNC/whatdataio/actions)
[![Coverage
Status](https://codecov.io/github/NCC-CNC/whatdataio/coverage.svg?branch=master)](https://codecov.io/github/NCC-CNC/whatdataio?branch=master)

### Overview

The *whatdataio R* package provides functions for importing and
exporting management action data. It provides consistent data format
standards for the [What To Do](https://github/NCC-CNC/whattodo) and
[What Template Maker](https://github/NCC-CNC/whattemplatemaker) web
applications. It is not designed for general purpose use outside of
these applications.

### Installation

The package is only available from an [online coding
repository](https://github.com/NCC-CNC/whatdataio). To install it, use
the following R code.

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("NCC-CNC/whatdataio")
```
