# ---
# repo: terminological/ggrrr
# file: standalone-stack-tools.R
# last-updated: '2025-09-21'
# license: https://unlicense.org
# imports:
# - rlang
# - testthat
# ---

#' Permissive as_function replacement.
#'
#' like `rlang::as_function` but only interprets functions or formulae
#' and ignores primitives and characters.
#'
#' @param fn something to interpret as a function or a primitive
#'
#' @returns the function or primitive
#' @keywords internal
#'
#' @examples
#' @unit
#'
#' f1 = .lax_as_function(tolower)
#' f2 = .lax_as_function(~ tolower(.x))
#' f3 = .lax_as_function("hello world")
#'
#' testthat::expect_equal(f1, tolower)
#' testthat::expect_equal(f2("HELLO WORLD"), f1("HELLO WORLD"))
#' testthat::expect_equal(f3, f1("HELLO WORLD"))
#'
.lax_as_function = function(fn) {
  try(if (is.function(fn)) return(fn), silent = TRUE)
  try(if (rlang::is_formula(fn)) return(rlang::as_function(fn)), silent = TRUE)
  return(fn)
}

#' Get the defined name of a function
#'
#' @param fn a function
#'
#' @returns the name as a string, as the function was defined
#' @keywords internal
#'
#' @examples
#' @unit
#'
#' f = tolower
#'
#' testthat::expect_equal(
#'   .get_fn_name(f),
#'   .get_fn_name(tolower)
#' )
#'
#' testthat::expect_equal(
#'   .get_fn_name(.get_fn_name),
#'   ".get_fn_name"
#' )
#'
#' testthat::expect_equal(
#'   .get_fn_name(function(x) x),
#'   "<unknown>"
#' )
#'
.get_fn_name = function(fn) {
  if (is.null(fn)) {
    return("<unknown>")
  }
  fnenv = as.list(rlang::fn_env(fn), all.names = TRUE)
  fnenv = fnenv[sapply(fnenv, is.function)]
  matches = sapply(fnenv, identical, fn)
  if (any(matches)) {
    return(paste0(names(fnenv)[matches], collapse = "/"))
  }
  return("<unknown>")
}


#' Search the call stack for a first parameter matching a specified class
#'
#' Finds the first instance of a function in the call stack that
#' was called with a object of `.class`. This can be used to find
#' dataframe parameters in tidy calls
#'
#' @param nframe where to start the search
#' @param .class the class name to find
#'
#' @returns a `.class` object
#' @keywords internal
#'
#' @examples
#' @unit
#'
#' h = function() {
#'  df = .search_call_stack(.class = "data.frame")
#'  return(nrow(df))
#' }
#'
#' g = function() {h()}
#'
#' f = function(x) {g()}
#'
#' testthat::expect_equal( f(iris), nrow(iris))
#'
.search_call_stack = function(
  .class,
  nframe = sys.nframe() - 1
) {
  frame = sys.frame(nframe)
  first_arg_name = names(formals(sys.function(nframe)))[[1]]
  try(
    {
      data = suppressWarnings(get(first_arg_name, envir = frame))
      if (inherits(data, .class)) return(data)
    },
    silent = TRUE
  )
  nframe = nframe - 1
  if (nframe < 1) {
    stop(sprintf("no %s found", .class))
  }
  .search_call_stack(.class, nframe)
}


#' Find the package environment for a given environment or function
#'
#' This searches the environment stack looking for the first environment
#' that defines a package. This is the package that the function is defined in.
#' If no namespace environment is found then the function is in the global
#' environment.
#'
#' @param env an environment or function.
#'
#' @returns a namespaced package environment
#' @keywords internal
#'
#' @examples
#' @unit
#'
#' env = .find_namespace(rlang::abort)
#' testthat::expect_equals(getNamespaceName(env),"rlang")
#'
#' f = function() {"test"}
#' testthat::expect_null(.find_namespace(f))
#'
.find_namespace = function(env = rlang::caller_env()) {
  if (rlang::is_function(env)) {
    env = rlang::fn_env(env)
  }
  while (!identical(env, emptyenv())) {
    try(
      {
        getNamespaceName(env)
        return(env)
      },
      silent = TRUE
    )
    env <- parent.env(env)
  }
  NULL
}
