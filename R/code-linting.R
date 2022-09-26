# from rex:::escape.character
.escape = function (x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[",
             "]", "{", "}", "\\")
  .sanitize(x, chars)
}

.sanitize = function (x, chars) {
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"),
       "\\\\\\1", x, perl = TRUE)
}

.move_safe = function(file, new_file = paste0(file,".old")) {
  if (!fs::file_exists(new_file)) {
    fs::file_move(file, new_file)
  } else {
    browser()
    .move_safe(file = new_file)
    fs::file_move(file, new_file)
  }
}

.write_safe = function(x, file) {
  if (fs::file_exists(file)) .move_safe(file)
  readr::write_lines(x,file)
}

.isGeneric = function(listOfFunctions) {
  unname(sapply(listOfFunctions, function(x) tryCatch(suppressWarnings(utils::isS3stdGeneric(x)), error = function(e) FALSE)))
}

#' Fix errors introduced in package creation by forgetting to qualify namespaces.
#'
#' This is a code linting function and not for general use.
#'
#' @param rDirectories the locations of the R code to fix
#' @param description the location of the description files
#' @param dry_run by default this function will not actually do anything unless this is set to FALSE.
#'
#' @return nothing. called for side effects.
#' @export
fix_unqualified_functions = function(rDirectories = c(here::here("R"),here::here("vignettes")), description = here::here("DESCRIPTION"), dry_run = TRUE ) {

  path = package = function_name = f = generic = content.old = name = value = content = changed = NULL

  files = dplyr::bind_rows(lapply(rDirectories, fs::dir_info)) %>% filter(fs::path_ext(path) %in% c("R","Rmd"))
  dMap = yaml::read_yaml(description)
  imports = dMap$Imports %>% stringr::str_split(",\\s+") %>% `[[`(1)
  loaded = (.packages())
  packages = unique(c(imports,loaded))
  packages = packages[!packages %in% c(dMap$Package,"base")]
  files = files %>% mutate(content.old = purrr::map(path, ~ readr::read_lines(.x)))
  packageMap = tibble::tibble(package = c("base",packages)) %>%
    mutate(function_name = purrr::map(package, ~ ls(asNamespace(.)))) %>%
    tidyr::unnest(function_name) %>%
    mutate( f = purrr::map2(function_name, package, function(f,p) {
      tryCatch(getFromNamespace(f,p), error = function(e) {function(){}})
    })) %>%
    mutate( generic = .isGeneric(f)) %>%
    select(-f)

  # functions from base or this package
  theseFunctions = c(ls(asNamespace(dMap$Package)),ls(asNamespace("base")))

  packageMap2 = packageMap %>% group_by(function_name) %>%
    mutate(package = ordered(package,levels = c("base",packages))) %>%
    arrange(desc(generic),package) %>%
    filter(row_number()==1) %>%
    filter(package != "base") %>%
    filter(!function_name %in% theseFunctions) %>%
    filter(stringr::str_detect(function_name,"[a-zA-Z]")) %>%
    filter(!is.na(function_name))

  files = files %>% mutate(content = content.old, matches=list(tibble()))

  for (pkg in packages) {
    # pkg = "dplyr"
    functions = packageMap2 %>% filter(package == pkg) %>% pull(function_name)

    if (length(functions) > 0) {
      functionRegex = paste0(lapply(functions, .escape),collapse = "|")
      functionRegex = paste0("([^:a-zA-Z0-9\\._])(",functionRegex,")\\(")
      replacement = paste0("\\1",pkg,"::\\2(")
      # c = files$content.old[[1]]

      getm = function(c) enframe(table(unlist(lapply(stringr::str_match_all(c,functionRegex), function(l) l[,3])))) %>% mutate(pkg = pkg, name = as.character(name), value=as.integer(value))
      #TODO
      # map the files$content using getm to get a per file list of matches for each package
      # present a option to the user describing what we are about to do for this package.

      files = files %>% mutate(
        content = purrr::map(content, ~ .x %>% stringr::str_replace_all(functionRegex, replacement)),
        matches = purrr::map2(content, matches, ~ bind_rows(.y,getm(.x) ))
      )
    }
  }

  files = files %>% mutate(changed = purrr::map2_lgl(content.old, content, ~ any(.x!=.y)))
  tmp = files %>% select(path,matches) %>% unnest(matches)

  if(any(files$changed)) {
    message(sum(tmp$value)," function calls missing namespaces found: ", paste0(unique(tmp$pkg), collapse = "; "))
    files %>% filter(changed) %>% purrr::pwalk(function(path,content.old,...) message(path))

    if(dry_run) {
      fixme = 3
    } else {
      fixme = utils::menu(c("Yes","No","Dry-run"), "Would you like me to fix these?")
    }
    dry_run = fixme==3

    if(fixme %in% c(1,3)) {

      if (dry_run) message("dry run: this is what would have been done")
      message("backing originals up to:")
      files %>% filter(changed) %>% purrr::pwalk(function(path,content.old,...) message("\t",path,".old"))
      if (!dry_run) files %>% filter(changed) %>% purrr::pwalk(function(path,content.old,...) .write_safe(content.old,paste0(path,".old")))

      message("writing modified files to: ")
      files %>% filter(changed) %>% purrr::pwalk(function(path,content,...) message("\t",path))
      if (!dry_run) files %>% filter(changed) %>% purrr::pwalk(function(path,content,...) readr::write_lines(content,path))
      if (dry_run) files %>% filter(changed) %>% purrr::pwalk(function(path,content,...) readr::write_lines(content,paste0(path,".dry_run")))
    }

  }


  nsMissing = tmp %>% filter(!(pkg %in% imports)) %>% pull(pkg) %>% unique()
  if(length(nsMissing) > 0) {
    message("Your DESCRIPTION file is missing packages that are currently loaded and used in your code. These are: ",paste(nsMissing,collapse = "; "))
    fixns = utils::menu(c("Yes","No"), "Would you like me to fix these?")
    if (fixns==1) {
      x = lapply(nsMissing, function(p) usethis::use_package(p))
    }
  }

  message("Done. You may want to run some tests before deleting the backup files.")
  return(files)
}
