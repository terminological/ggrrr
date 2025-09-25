# ggrrr

Data presentation and visualisation hacks.

[![R-CMD-check](https://github.com/terminological/ggrrr/workflows/R-CMD-check/badge.svg)](https://github.com/terminological/ggrrr/actions)
[![DOI](https://zenodo.org/badge/489738724.svg)](https://zenodo.org/badge/latestdoi/489738724)
[![ggrrr status badge](https://terminological.r-universe.dev/badges/ggrrr)](https://terminological.r-universe.dev)

see the [package documentation](https://terminological.github.io/ggrrr/docs/)

`ggrrr` is a repository of utility functions. It is not primarily designed to be
installed, but rather as a source of standalone library files - ideally in 
conjunction with `pkgtools` which provides some infrastructure to render unit
tests, and interact with local standalone files. 

```R
# Optional:
# Enable repository from terminological
options(repos = c(
  terminological = 'https://terminological.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
install.packages("pkgtools")

# Use the `ggrrr` repository as a source of standalones
pkgtools::use_standalone("terminological/ggrrr")

# Alternatively:
usethis::use_standalone("terminological/ggrrr") 
# will work in a package project.
```

Most of the functions are available in standalone format for inclusion into your
own package or analysis project. There are some functions that are only
available in `ggrrr` as a package - particularly font installation functions. For 
these you will have to install `ggrrr`:

```R
# Enable repository from terminological
options(repos = c(
  terminological = 'https://terminological.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
install.packages("ggrrr")

# Or the unstable github version
devtools::install_github("terminological/ggrrr")
```
`ggrrr` is not on CRAN. It is not likely to ever be and the use of standalones
is the primary channel for distribution.

`ggrrr` has a focus on output from `ggplot` and `huxtable` with file size, image
resolution, opinionated formatting of defaults, and consistent output and sizing
between screen, html, pdf and png outputs.

It also addresses an eclectic mix of other issues that arose during my time
developing with R.

* Font handling - a million and one ways of doing it in R. Now a million and two.
* writing files to date-versioned output directories, and constructing data supplements.
* caching intermediate analysis stages.
* outputting the right thing at the right time depending on whether you are
running code in the terminal, knitting a markdown document etc.
* converting tidy data to tabular content.
* embedding tables into ggplot.
* putting source code into html and pdf documents.
* random scales I found useful.
* mixture normal distributions for combining bootstraps
* tidy data utility functions such as cutting dates into regular periods,
cutting integer values into labelled categories.

## Pre installation:

`ggrrr` will fallback to use of a headless chrome instance in some situations,
particularly for rendering very complicated ggplots and huxtables to pdf.
`ggrrr` is configured to use `html2pdfr` for html to pdf conversion if
available. This uses `rJava`, which in turn requires a `Java` installation. In
theory it is optional and will not be installed automatically, or everything
will be installed automagically. In practice getting a working `html2pdfr` is
probably done separately by following the `html2pdfr` [installation
instructions](https://github.com/terminological/html2pdfr).
