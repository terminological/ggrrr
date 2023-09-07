## Fonts ----

#' Which fonts are available on this system.
#'
#' @param family a font family name or names
#'
#' @return the font family name if it can be located or an error otherwise
#' @export
#' @examples
#' fonts_available(c("Arial","sdfdsfsd"))
fonts_available = function(family = NULL) {
  families = c(systemfonts::registry_fonts()$family, systemfonts::system_fonts()$family)

  if (is.null(family)) {
    return(sort(unique(families)))
  } else {
    return(family[family %in% families])
  }
}

#' Ensures a font is available.
#'
#' This checks to see if a font exists. If missing it will try and install from google fonts.
#' If this is not possible it will throw an error.
#'
#' @param family a font family name or names
#' @param sub substitue missing fonts?
#'
#' @return the font family name if it can be located or an error otherwise
#' @export
#'
#' @examples
#' check_font("Roboto")
check_font = function(family, sub=TRUE) {

  path = NULL

  tmp = register_web_fonts(family)
  .update_web_font_option(tmp$webfonts)
  missing = family[family %in% tmp$unmatched]

  if (!sub) {
    if (length(tmp$unmatched) > 0) stop("missing font family/ies: ",paste0(tmp$unmatched, collapse=","))
    return(family)
  }

  paths = sapply(missing, function(x) systemfonts::match_font(x)$path)
  substitues = dplyr::bind_rows(
    systemfonts::system_fonts() %>% dplyr::select(path,family),
    systemfonts::registry_fonts() %>% dplyr::select(path,family)
  ) %>% dplyr::filter(path %in% paths) %>% dplyr::pull(family)

  if (length(missing) > 0) message("substitute fonts: ",paste0(substitues,collapse=", "), " for ", paste0(missing,collapse=", "))

  return(unique(c(family[!family %in% tmp$unmatched],substitues)))

}

#' Reset any custom webfonts
#'
#' @return nothing
#' @export
reset_fonts = function() {
  options("ggrrr.webfonts" = list())
  systemfonts::clear_registry()
}

.get_web_font_option = function() {
  return(getOption("ggrrr.webfonts", default=list()))
}

.update_web_font_option = function(webfonts) {
  options("ggrrr.webfonts" = unique(c(.get_web_font_option(), webfonts)))
  invisible(NULL)
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
#       regular <- utils::head(fd$afmfile[!fd$Bold & !fd$Italic],1)
#       bold <- utils::head(fd$afmfile[fd$Bold & !fd$Italic],1)
#       italic <- utils::head(fd$afmfile[!fd$Bold & fd$Italic],1)
#       bolditalic <- utils::head(fd$afmfile[fd$Bold & fd$Italic],1)
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


## adapted from showtextdb ----

.esc = function(names) {
  stringr::str_replace_all(names,"[[:space:]]", "+")
}

webfont_provider = list(
  # google = function(fonts) {
  #   sprintf(
  #     "https://fonts.googleapis.com/css?family=%s",
  #     paste0(sprintf(
  #       "%s:r,b,i,bi", .esc(fonts)
  #     ),collapse="|")
  #   )
  # },
  google = function(fonts) {
    sprintf(
      "https://fonts.googleapis.com/css2?%s",
      paste0(sprintf(
        "family=%s:ital,wght@0,400;0,700;1,400;1,700", .esc(fonts)
      ),collapse="&")
    )
  },
  brick = function(fonts) {
    sprintf(
      "https://brick.freetls.fastly.net/%s",
      paste0(sprintf(
        "%s:400,400i,700,700i", .esc(fonts)
      ),collapse="/")
    )
  }
)

# .unquote(c("ok","'ok'","\"ok\""))
.unquote = function(s) {
  stringr::str_extract(s, "^('|\")?([^'\"]*)('|\")?$", group = 2)
}

.style_to_weight = function(style) {
  dplyr::case_when(
    stringr::str_detect(tolower(style), stringr::fixed("bold")) ~ "700",
    stringr::str_detect(tolower(style), stringr::fixed("regular")) ~ "400",
    stringr::str_detect(tolower(style), stringr::fixed("normal")) ~ "400",
    stringr::str_detect(tolower(style), stringr::fixed("thin")) ~ "100",
    stringr::str_detect(tolower(style), stringr::fixed("extra light")) ~ "200",
    stringr::str_detect(tolower(style), stringr::fixed("light")) ~ "300",
    stringr::str_detect(tolower(style), stringr::fixed("book")) ~ "350",
    stringr::str_detect(tolower(style), stringr::fixed("medium")) ~ "500",
    stringr::str_detect(tolower(style), stringr::fixed("semi bold")) ~ "600",
    stringr::str_detect(tolower(style), stringr::fixed("extra bold")) ~ "800",
    stringr::str_detect(tolower(style), stringr::fixed("black")) ~ "900",
    tolower(style) == "italic" ~ "400",
    tolower(style) == "oblique" ~ "400",
    TRUE ~ NA_character_)
}

.style_to_style = function(style) {
  dplyr::case_when(
    stringr::str_detect(tolower(style), stringr::fixed("italic")) ~ "italic",
    stringr::str_detect(tolower(style), stringr::fixed("oblique")) ~ "italic",
    TRUE ~ "normal")
}

#' Find fonts in webfont providers
#'
#' Caches, registers and constructs webfont CSS font face directives.
#'
#' @param fonts a set of fonts as a character vector
#' @param services one of `r paste0("'",names(webfont_provider),"'",collapse=", ")`
#' @param ... not used
#'
#' @return a list containing the following:
#' * webfonts - a list of `svglite::font_face` directives for use in embedding
#' into svg files using the `svglite::svglite(web_fonts=...)` option.
#' * matched - a dataframe of fonts found in the services
#' * unmatched - a vector of fonts not matched online.
#'
#' @export
#'
#' @examples
#' tmp = register_web_fonts("Kings")
#' tmp$webfonts
#' systemfonts::registry_fonts()
register_web_fonts = function(fonts, services = names(webfont_provider), ...) {
  name = path = .  = NULL
  services = match.arg(services, several.ok = TRUE)
  deferred = fonts
  found = tibble::tibble(name = character(),
    family = character(), weight = character(), style = character(), url = character(),
    format=character(), unicode_range=character(), face = character()
  )
  for (service in services) {
    request_url = webfont_provider[[service]](deferred)
    res = tryCatch({
      ret = suppressMessages(suppressWarnings(cache_download(request_url, .stale = 7, quiet=TRUE)))
      res = readr::read_file(ret)
    }, error=function(e) "" )
    matched = stringr::str_detect(res, sapply(deferred, sprintf, fmt="@font-face.*\\{[^\\}]*font-family:\\s?'%s'"))

    info = stringr::str_match_all(res, "@font-face\\s?\\{([^\\}]+)\\}")[[1]][,2]
    info = info[info != ""]
    family = stringr::str_extract(info, "font-family:[[:space:]]*([^:;]+);",group=1) %>% .unquote()

    supplied_name = names(fonts)[unlist(sapply(family, match, deferred[matched]))]
    deferred = deferred[!matched]

    style = stringr::str_extract(info, "font-style:[[:space:]]*([^:;]+);", group=1)
    unicode_range = stringr::str_extract(info, "unicode-range:[[:space:]]*([^:;]+);", group=1)
    weight = stringr::str_extract(info, "font-weight:[[:space:]]*([[:digit:]]+);", group=1)
    face = paste(style, weight, sep = "")
    face[face == "normal400"] = "plain"
    face[face == "normal700"] = "bold"
    face[face == "italic400"] = "italic"
    face[face == "italic700"] = "bolditalic"
    url = stringr::str_extract(info, "url\\(([^\\)]+)\\)", group=1) %>% stringr::str_replace("^//", "https://")
    format = stringr::str_extract(info, "format\\(([^\\)]+)\\)", group=1) %>% .unquote()
    format[format == "truetype"] = "ttf"
    format[format == "opentype"] = "otf"
    format[format == "embedded-opentype"] = "eof"
    local = stringr::str_extract(info, "local\\(([^\\)]+)\\).*", group=1) %>% .unquote()

    if(is.null(supplied_name)) supplied_name = family # ifelse(is.na(local), family, local)

    # format = gsub(".*format\\(([^()]+)\\).*", "\\1", info)
    found = found %>% dplyr::bind_rows(
      tibble::tibble(
        name = supplied_name, family = family, weight = weight, style=style, url = url, format=format,
        unicode_range = unicode_range, face=face
      )
    )
    if (length(deferred) == 0) break;
  }
  unmatched = deferred[!deferred %in% systemfonts::system_fonts()$family]

  if (nrow(found) > 0) {
    sysf = systemfonts::system_fonts() %>%
      dplyr::mutate(
        weight = .style_to_weight(style),
        style = .style_to_style(style)
      )
    # download and cache
    present = found %>% dplyr::select(-name) %>% dplyr::inner_join(sysf %>% dplyr::select(name,family,path,style,weight), by=c("family","style","weight"))
    missing = found %>%
      dplyr::anti_join(present, by="url") %>%
      dplyr::mutate(
        path = suppressMessages(purrr::map_chr(url, cache_download, quiet=TRUE, .progress = TRUE))
      )
    # register in systemfonts
    registered = missing %>% dplyr::select(name,face,path) %>%
      tidyr::pivot_wider(names_from = face, values_from = path) %>%
      dplyr::mutate(
        dplyr::across(tidyselect::everything(), ~ ifelse(is.na(.x), plain, .x) )
      ) %>%
      dplyr::mutate(
        err = purrr::pmap_chr(., .f = function(...) {tryCatch(systemfonts::register_font(...) %||% "ok", error = function(e) e$message)})
      )
    regf = systemfonts::registry_fonts() %>%
      dplyr::mutate(
        weight = .style_to_weight(style),
        style = .style_to_style(style)
      )
    webfonts = dplyr::bind_rows(
        present,
        missing %>% dplyr::semi_join(regf, by=c("path"))
      ) %>%
      dplyr::select(local = name, family, weight, style, format, url) %>%
      tidyr::pivot_wider(names_from = format, values_from = url) %>%
      purrr::pmap(.f = svglite::font_face)

    return(list(
      webfonts = webfonts,
      matched = found,
      unmatched = unmatched
    ))

  }
  return(list(
    webfonts = list(),
    matched = found,
    unmatched = unmatched
  ))
}
