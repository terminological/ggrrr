## Fonts ----

# TODO:
# Google font download, cache on local persistent cache -
# Unzip and ?extrafont::ttf_import
# loadfonts


#' Version of loadfonts from extrafonts that uses first font file it finds rather than failing if multiple are found
#'
#' @param device The output device. Can be "pdf" (the default), "postscript", or "win".
#' @param quiet If FALSE, print a status message as each font is registered. If TRUE, don't print.
#'
#' @return
#' @export
loadfonts = function (device = "pdf", quiet = FALSE) {
  fontdata <- extrafont::fonttable()
  if (device == "pdf") {
    cfonts <- names(grDevices::pdfFonts())
    ffname <- "pdfFonts"
  }
  else if (device == "postscript") {
    cfonts <- names(grDevices::postscriptFonts())
    ffname <- "postscriptFonts"
  }
  else if (device == "win") {
    cfonts <- names(grDevices::windowsFonts())
    ffname <- "windowsFonts"
  }
  else {
    stop("Unknown device: ", device)
  }
  fontfunc <- match.fun(ffname)
  if (device %in% c("pdf", "postscript")) {
    for (family in unique(fontdata$FamilyName)) {
      if (family %in% cfonts) {
        if (!quiet)
          message(family, " already registered with ",
                  ffname, "().")
        (next)()
      }
      fd <- fontdata[fontdata$FamilyName == family, ]
      regular <- head(fd$afmfile[!fd$Bold & !fd$Italic],1)
      bold <- head(fd$afmfile[fd$Bold & !fd$Italic],1)
      italic <- head(fd$afmfile[!fd$Bold & fd$Italic],1)
      bolditalic <- head(fd$afmfile[fd$Bold & fd$Italic],1)
      if (length(regular) == 0) {
        if (!quiet) {
          message("No regular (non-bold, non-italic) version of ",
                  family, ". Skipping setup for this font.")
        }
        (next)()
      }
      if (length(bold) == 0)
        bold <- regular
      if (length(italic) == 0)
        italic <- regular
      if (length(bolditalic) == 0)
        bolditalic <- bold
      if (!is.na(fd$afmsymfile[1]) && fd$afmsymfile[1] !=
          "" && all(fd$afmsymfile[1] == fd$afmsymfile)) {
        symbol <- fd$afmsymfile[1]
      }
      else {
        symbol <- NULL
      }
      if (!quiet)
        message("Registering font with R using ", ffname,
                "(): ", family)
      args <- list()
      args[[family]] <- grDevices::Type1Font(family, metrics = file.path(extrafont:::metrics_path(),
                                                                         c(regular, bold, italic, bolditalic, symbol)))
      do.call(fontfunc, args)
    }
  }
  else if (device == "win") {
    for (family in unique(fontdata$FamilyName)) {
      if (family %in% cfonts) {
        if (!quiet)
          message(family, " already registered with ",
                  ffname, "().")
        (next)()
      }
      fd <- fontdata[fontdata$FamilyName == family, ]
      if (!quiet)
        message("Registering font with R using ", ffname,
                "(): ", family)
      args <- list()
      args[[family]] <- grDevices::windowsFont(family)
      do.call(fontfunc, args)
    }
  }
}
