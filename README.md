# ggrrr

Data presentation and visualisation hacks.

see the [package documentation](https://terminological.github.io/ggrrr/docs/)

`ggrrr` fixes a few annoying problems in generating publication ready figures and tables.
It pays particular attention to output file size and image resolution, with opinionated formatting of defaults, with consistent sizing
between screen, html and pdf outputs.

It also addresses an eclectic mix of other issues that arose during my time developing with R.

* Font handling - a million and one ways of doing it in R. Now a million and two.
* loading dependencies when you are actively developing libraries in parallel.
* writing files to date-versioned output directories, and constructing data supplements.
* caching intermediate analysis stages.
* outputting the right thing at the right time depending on whether you are running code in the terminal, knitting a markdown document etc.
* converting tidy data to tabular content.
* embedding tables into ggplot.
* putting source code into html and pdf documents.

`ggrrr` is not on CRAN you can install the most up to date version with:

```R
devtools::install_github("terminological/ggrrr@0.0.0.9008")
```

It is pretty unstable and under continuous parallel development which may break things without warning.
It is sensible to use a specific tag or released version, unless you are me.
It is in evolution as is its documentation (i.e. documentation is not very good)
