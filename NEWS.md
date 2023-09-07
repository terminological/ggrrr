# ggrrr 0.0.0.9006

* Starting trying to be better at documenting changes
* Switched from extrafont to sysfonts / systemfonts
* html2pdfr and webshot made optional (as suggests)
* automating font dowload from google fonts when missing.
* caching and cache downloads
* pkgdown site.

# ggrrr 0.0.0.9008

* major documentation improvements (i.e. there is some)
* Showtext integration / extrafont removed - N.B. Switch to showtext from extrafonts introduced utf8 support regression, may have to consider a hybrid model with showtext for png, or pdf first with pdftools conversion for png.
* GGPlot scales included
* date and timepoints, conversion, cut and full_seq examples using dated periods as an alternative.
* integer values cutting to named factors.

# ggrrr 0.0.0.9009

* Fixed CI for mac and windows. Updated documentation
* Cairo dependency for xquartz on mac
* no mulitarch on windows

# ggrrr 0.0.0.9012

* reinstalled sinew alternative fix_qualified_functions
* fdmy for formatting a date
* optional_fn for hiding references to potentially non installed functions

# ggrrr 0.0.0.9013

* Documentation tweaks

# ggrrr 0.0.0.9014

* fix anomalous dependencies

# ggrrr 0.0.0.9017

* big tidy up of rendering pipelines
* remove showtext etc in favour of svglite
