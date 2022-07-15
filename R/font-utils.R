## Fonts ----

#' Downloads caches and installs a font family from google
#'
#' This checks if the file is already downloaded and if not pull the font file from google fonts.
#' If it does not exist it will throw an error. Once downloaded a font will be made available through
#' `showtext` and through `systemfonts` font packages.
#'
#' @param family the google font family (e.g. Roboto) as found here: https://fonts.google.com/ (case sensitive)
#' @param ... not used
#'
#' @return nothing
#' @export
google_font = function(family, ...) {
  if (!family %in% c(sysfonts::font_files()$family, showtextdb::font_installed())) {
      # google_url = paste0("https://fonts.google.com/download?family=",family)
      # zippath = cache_download(url=google_url, .cache=rappdirs::user_cache_dir("google-fonts"))
      # unzippath = paste0(fs::path_ext_remove(zippath),"-unzip")
      # fs::dir_create(unzippath)
      # zip::unzip(zippath,exdir=unzippath)

      # This is unnecessary as everything in sysfonts is available to showtext
      # if (family %in% sysfonts::font_files()$family) {
      #   thisfamily = family
      #   finst = sysfonts::font_files() %>%
      #     filter(family==thisfamily) %>%
      #     group_by(face) %>%
      #     filter(row_number()==1) %>%
      #     mutate(face = stringr::str_remove_all(tolower(face),"[^a-z]")) %>%
      #     mutate(path=paste0("file://localhost",path.expand(file.path(path,file)))) %>%
      #     select(showtext_name = family,face,path) %>%
      #     pivot_wider(names_from = face, values_from = path, names_glue = "{face}_url") %>%
      #     mutate(font_ext = "ttf") %>% as.list()
      #   showtextdb::font_install(finst)
      # } else {
    inst = showtextdb::google_fonts(name = family)
    showtextdb::font_install(inst)
    showtextdb::load_showtext_fonts()
  }

  if (!family %in% c(sysfonts::font_files()$family, showtextdb::font_installed())) {
    stop(family, " cannot be found in sysfonts")
  }

  maybe = function(x) if(x=="") NULL else x

  if (!(family %in% c(systemfonts::registry_fonts()$family, systemfonts::system_fonts()$family)) &
      (family %in% showtextdb::font_installed())) {
    systemfonts::register_font(
      family,
      plain = system.file("fonts",family,"regular.ttf",package = "showtextdb"),
      bold = system.file("fonts",family,"bold.ttf",package = "showtextdb") %>% maybe(),
      italic = system.file("fonts",family,"italic.ttf",package = "showtextdb") %>% maybe(),
      bolditalic = system.file("fonts",family,"bolditalic.ttf",package = "showtextdb") %>% maybe()
    )
  }

  if (!family %in% c(systemfonts::registry_fonts()$family, systemfonts::system_fonts()$family)) {
    stop(family, " cannot be found in systemfonts")
  }

  return(family)
}

#' Check a font exists on the system, or list all fonts available
#'
#' Given a font family this checks it si locally available in both the
#' `showtext` and `systemfonts` subsystems
#'
#' @param family an optional list of fonts to check for
#'
#' @return a list of available fonts
#' @export
#'
#' @examples
#' fonts_available(c("Arial","sdfdsfsd"))
fonts_available = function(family = NULL) {
  families = base::intersect(
    c(showtextdb::font_installed(), sysfonts::font_files()$family),
    c(systemfonts::registry_fonts()$family, systemfonts::system_fonts()$family)
  )

  if (is.null(family)) {
    return(sort(unique(families)))
  } else {
    return(family[family %in% families])
  }
}

.local_font_details = function(family, url=FALSE) {
  thisfamily = family
  finst = sysfonts::font_files() %>%
    filter(family==thisfamily) %>%
    group_by(face) %>%
    filter(row_number()==1) %>%
    mutate(face = stringr::str_remove_all(tolower(face),"[^a-z]"))
  if (url) {
    finst = finst %>% mutate(path=paste0("file://localhost",path.expand(file.path(path,file))))%>%
      select(showtext_name = family,face,path) %>%
      pivot_wider(names_from = face, values_from = path, names_glue = "{face}_url") %>%
      mutate(font_ext = "ttf") %>%
      as.list()
  } else {
    finst = finst %>% mutate(path=path.expand(file.path(path,file))) %>%
      select(family,face,path) %>%
      pivot_wider(names_from = face, values_from = path, names_glue = "{face}") %>%
      as.list()
  }
  return(finst)
}

#' Ensures a font is available.
#'
#' This checks to see fi a font exists. If missing it will try and install from google fonts.
#' If this is not possible it will throw an error.
#'
#' @param family a font family name
#'
#' @return the font family name if it can be located or an error otherwise
#' @export
#'
#' @examples
#' check_font("Roboto")
check_font = function(family) {
  if (!family %in% showtextdb::font_installed()) {
    if (family %in% sysfonts::font_files()$family) {
      finst = .local_font_details(family,url = TRUE)
      showtextdb::font_install(finst)
    } else {
      family = tryCatch({
        google_font(family)
      }, error = function(e) stop("Font family not found locally or on Google fonts: ",family," try something from fonts_available()."))
    }
  }
  showtextdb::load_showtext_fonts()
  return(family)
}

.font_paths = function() {
  unique(c(sysfonts::font_paths(), system.file("fonts", package="showtextdb")))
}

# Version of loadfonts from extrafonts that uses first font file it finds rather than failing if multiple are found
#
# @param device The output device. Can be "pdf" (the default), "postscript", or "win".
# @param quiet If FALSE, print a status message as each font is registered. If TRUE, don't print.
#
# @return nothing. called for side effects
# @export
# loadfonts = function (device = "pdf", quiet = FALSE) {
#   fontdata <- extrafont::fonttable()
#   if (device == "pdf") {
#     cfonts <- names(grDevices::pdfFonts())
#     ffname <- "pdfFonts"
#   }
#   else if (device == "postscript") {
#     cfonts <- names(grDevices::postscriptFonts())
#     ffname <- "postscriptFonts"
#   }
#   else if (device == "win") {
#     cfonts <- names(grDevices::windowsFonts())
#     ffname <- "windowsFonts"
#   }
#   else {
#     stop("Unknown device: ", device)
#   }
#   fontfunc <- match.fun(ffname)
#   if (device %in% c("pdf", "postscript")) {
#     for (family in unique(fontdata$FamilyName)) {
#       if (family %in% cfonts) {
#         if (!quiet)
#           message(family, " already registered with ",
#                   ffname, "().")
#         (next)()
#       }
#       fd <- fontdata[fontdata$FamilyName == family, ]
#       regular <- head(fd$afmfile[!fd$Bold & !fd$Italic],1)
#       bold <- head(fd$afmfile[fd$Bold & !fd$Italic],1)
#       italic <- head(fd$afmfile[!fd$Bold & fd$Italic],1)
#       bolditalic <- head(fd$afmfile[fd$Bold & fd$Italic],1)
#       if (length(regular) == 0) {
#         if (!quiet) {
#           message("No regular (non-bold, non-italic) version of ",
#                   family, ". Skipping setup for this font.")
#         }
#         (next)()
#       }
#       if (length(bold) == 0)
#         bold <- regular
#       if (length(italic) == 0)
#         italic <- regular
#       if (length(bolditalic) == 0)
#         bolditalic <- bold
#       if (!is.na(fd$afmsymfile[1]) && fd$afmsymfile[1] !=
#           "" && all(fd$afmsymfile[1] == fd$afmsymfile)) {
#         symbol <- fd$afmsymfile[1]
#       }
#       else {
#         symbol <- NULL
#       }
#       if (!quiet)
#         message("Registering font with R using ", ffname,
#                 "(): ", family)
#       args <- list()
#       args[[family]] <- grDevices::Type1Font(family, metrics = file.path(metrics_path(), c(regular, bold, italic, bolditalic, symbol)))
#       do.call(fontfunc, args)
#     }
#   }
#   else if (device == "win") {
#     for (family in unique(fontdata$FamilyName)) {
#       if (family %in% cfonts) {
#         if (!quiet)
#           message(family, " already registered with ",
#                   ffname, "().")
#         (next)()
#       }
#       fd <- fontdata[fontdata$FamilyName == family, ]
#       if (!quiet)
#         message("Registering font with R using ", ffname,
#                 "(): ", family)
#       args <- list()
#       args[[family]] <- grDevices::windowsFont(family)
#       do.call(fontfunc, args)
#     }
#   }
# }
#
# metrics_path = function() {
#   file.path(system.file(package = "extrafontdb"),"metrics")
# }
#
# ttf_import = function (paths = NULL, recursive = TRUE, pattern = NULL, family)
# {
#   if (is.null(paths))
#     paths <- ttf_find_default_path()
#   ttfiles <- normalizePath(list.files(paths, pattern = "\\.ttf$",
#                                       full.names = TRUE, recursive = recursive, ignore.case = TRUE))
#   if (!is.null(pattern)) {
#     matchfiles <- grepl(pattern, basename(ttfiles))
#     ttfiles <- ttfiles[matchfiles]
#   }
#   message("Scanning ttf files in ", paste(paths, collapse = ", "),
#           " ...")
#   fontmap <- ttf_extract(ttfiles,family)
#   fontmap <- fontmap[!is.na(fontmap$FontName), ]
#   message("Found FontName for ", nrow(fontmap), " fonts.")
#   afmdata <- extrafont:::afm_scan_files()
#   fontdata <- merge(fontmap, afmdata)
#   if (nrow(fontdata) > 0) {
#     fontdata$package <- NA
#     fonttable_add(fontdata)
#   }
# }
#
# ttf_extract = function (ttfiles, family = "Unknown")
# {
#   message("Extracting .afm files from .ttf files...")
#   fontdata <- data.frame(fontfile = ttfiles, FontName = "",
#                          stringsAsFactors = FALSE)
#   outfiles <- file.path(metrics_path(), sub("\\.ttf$", "",
#                                             basename(ttfiles), ignore.case = TRUE))
#   dir.create(file.path(tempdir(), "fonts"), showWarnings = FALSE)
#   tmpfiles <- file.path(tempdir(), "fonts", sub("\\.ttf$",
#                                                 "", basename(ttfiles), ignore.case = TRUE))
#   ttf2pt1 <- Rttf2pt1::which_ttf2pt1()
#   if (.Platform$OS.type == "windows")
#     args <- c("-a", "-G", "fAe")
#   else args <- c("-a", "-GfAe")
#   for (i in seq_along(ttfiles)) {
#     message(ttfiles[i], appendLF = FALSE)
#     ret <- system2(enc2native(ttf2pt1), c(args, shQuote(ttfiles[i]),
#                                           shQuote(tmpfiles[i])), stdout = TRUE, stderr = TRUE)
#     fontnameidx <- grepl("^FontName ", ret)
#     if (sum(fontnameidx) == 0) {
#       fontname <- family
#     }
#     else if (sum(fontnameidx) == 1) {
#       fontname <- sub("^FontName ", "", ret[fontnameidx])
#     }
#     else if (sum(fontnameidx) > 1) {
#       warning("More than one FontName found for ", ttfiles[i])
#     }
#     if (fontname == "" || fontname == "Unknown") {
#       fontdata$FontName[i] <- NA
#       message(" : No FontName. Skipping.")
#     }
#     else if (fontname %in% extrafont::fonttable()$FontName) {
#       fontdata$FontName[i] <- NA
#       message(" : ", fontname, " already registered in fonts database. Skipping.")
#     }
#     else {
#       fontdata$FontName[i] <- fontname
#       extrafont:::gzcopy_exclude(paste(tmpfiles[i], ".afm", sep = ""),
#                      paste(outfiles[i], ".afm.gz", sep = ""), delete = TRUE,
#                      exclusions = c("^Characters"))
#       message(" => ", paste(outfiles[i], sep = ""))
#     }
#   }
#   return(fontdata)
# }
