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

fix_unqualified_functions = function(rDirectories = c(here::here("R"),here::here("vignettes")), description = here::here("DESCRIPTION") ) {

  devtools::load_all(here::here())

  files = dplyr::bind_rows(lapply(rDirectories, fs::dir_info)) %>% filter(fs::path_ext(path) %in% c("R","Rmd"))
  dMap = yaml::read_yaml(description)
  imports = dMap$Imports %>% stringr::str_split(",\\s+") %>% `[[`(1)
  loaded = (.packages())
  packages = unique(c(loaded,imports))
  packages = packages[!packages %in% c(dMap$Package,"base")]
  files = files %>% mutate(content.old = purrr::map(path, ~ readr::read_lines(.x)))
  theseFunctions = ls(asNamespace(dMap$Package))

  files = files %>% mutate(content = content.old, matches=list(tibble()))

  for (pkg in packages) {
    # pkg = "dplyr"
    functions = ls(asNamespace(pkg))
    functions = functions[!functions %in% theseFunctions]

    functionRegex = paste0(lapply(functions, rex::escape),collapse = "|")
    functionRegex = paste0("([^:a-zA-Z0-9\\._])(",functionRegex,")\\(")
    replacement = paste0("\\1",pkg,"::\\2(")
    # c = files$content.old[[1]]

    getm = function(c) enframe(table(unlist(lapply(stringr::str_match_all(c,functionRegex), function(l) l[,3])))) %>% mutate(pkg = pkg, name = as.character(name), value=as.integer(value))

    files = files %>% mutate(
      content = purrr::map(content, ~ .x %>% stringr::str_replace_all(functionRegex, replacement)),
      matches = purrr::map2(content, matches, ~ bind_rows(.y,getm(.x) ))
    )
  }

  files = files %>% mutate(changed = purrr::map2_lgl(content.old, content, ~ any(.x!=.y)))

  files %>% filter(changed) %>% purrr::pwalk(function(path,content.old,...) .write_safe(content.old,paste0(path,".old")))
  files %>% filter(changed) %>% purrr::pwalk(function(path,content,...) readr::write_lines(content,path))

  files %>% select(path,matches) %>% unnest(matches)

}
