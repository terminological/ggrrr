# ggrrr

Data presentation and visualisation hacks.

[![R-CMD-check](https://github.com/terminological/ggrrr/workflows/R-CMD-check/badge.svg)](https://github.com/terminological/ggrrr/actions)
[![DOI](https://zenodo.org/badge/489738724.svg)](https://zenodo.org/badge/latestdoi/489738724)
[![ggrrr status badge](https://terminological.r-universe.dev/badges/ggrrr)](https://terminological.r-universe.dev)

see the [package documentation](https://terminological.github.io/ggrrr/docs/)

`ggrrr` fixes a few annoying problems in generating publication ready figures and tables.

It pays particular attention to output file size and image resolution, with
opinionated formatting of defaults, with consistent output and sizing between
screen, html, pdf and png outputs.

It also addresses an eclectic mix of other issues that arose during my time developing with R.

* Font handling - a million and one ways of doing it in R. Now a million and two.
* loading dependencies when you are actively developing libraries in parallel.
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

`ggrrr` uses cairo graphics if available. This need to be installed which should
happen automatically. However on macOS cairo has a transitive dependency on
`XQuartz` which is annoyingly not automatic. Therefore on macOS `XQuartz` needs
to be installed manually from https://www.xquartz.org/. or alternatively do a
`brew install --cask xquartz`, before the automatically installed cairo can
work.

`ggrrr` is configured to use `html2pdfr` for html to pdf conversion if
available. This uses `rJava`, which in turn requires a `Java` installation. In
theory it is optional and will not be installed automatically, or everything
will be installed automagically. In practice getting a working `html2pdfr` is
probably done separately by following the `html2pdfr` [installation
instructions](https://github.com/terminological/html2pdfr). If this is too much
trouble then an installation of `webshot` via CRAN should be easier (N.b. not
`webshot2`).

`ggrrr` is not on CRAN you can install the most up to date release with:

```R
# Enable repository from terminological
options(repos = c(
  terminological = 'https://terminological.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install ggrrr in R
install.packages('ggrrr')
```

Unstable versions are available on the main branch in github:

```R
# The unstable head verions
devtools::install_github("terminological/ggrrr")

# As ggrrr is quite fluid, getting a specific version is recommended for any 
# particular analysis
devtools::install_github("terminological/ggrrr@0.0.0.9009")

```

It is pretty unstable and under continuous parallel development which may break
things without warning. It is sensible to use a specific tag or released
version, unless you are me.
