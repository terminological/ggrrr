# look for a dataframe as the first argument in the call stack
.search_call_stack = function(nframe = sys.nframe()-1) {
  frame = sys.frame(nframe)
  first_arg_name = ls(frame)[1]
  try({
    data = suppressWarnings(first_arg_name %>% get(envir=frame))
    if(is.data.frame(data)) return(data)
  })
  nframe = nframe-1
  if (nframe < 1) stop("no data frame found")
  .search_call_stack(nframe)
}


#' Reuse tidy-select syntax outside of a tidy-select function
#'
#' @param tidyselect a tidyselect syntax which will be evaluated in context by looking for a call in the call stack that includes a dataframe as the first argument
#' @param data (optional) a specific dataframe with which to evaluate the tidyselect
#'
#' @return a list of symbols resulting from the evaluation of the tidyselect in the context of the current call stack (or a provided data frame)
#' @export
as_vars = function(tidyselect, data=NULL) {
  expr = enquo(tidyselect)
  if(is.null(data)) data = .search_call_stack()
  res = tidyselect::eval_select(expr,data)
  lapply(names(res), as.symbol)
}

## Dataset description helpers ----


#' This function examines a dataframe and returns a list of the columns with sub-lists as all the options for factors
#'
#' @param df a dataframe to examine
#'
#' @return a list of lists with the column name and the factor levels as list
#' @export
get_value_sets = function(df) {
  v = lapply(colnames(df), function(x) {
    if (all(is.na(df[[x]]))) {
      node = list(empty = TRUE)
    } else if (is.factor(df[[x]])) {
      node = as.list(levels(df[[x]]))
      names(node) = node
    } else if (is.character(df[[x]])) {
      tmp = unique(df[[x]])
      # TODO: sort by frequency
      node = as.list(tmp)
      names(node) = node
    } else if (is.numeric(df[[x]])) {
      node = list(
        min = min(df[[x]],na.rm = TRUE),
        max = max(df[[x]],na.rm = TRUE),
        mean = mean(df[[x]],na.rm = TRUE)
      )
    } else {
      node = list()
    }
    class(node) = c("checked_list",class(node))
    return(node)
  })
  names(v) = colnames(df)
  class(v) = c("checked_list",class(v))
  return(v)
}

# This is a sub class of list that throws an error if you attempt to access a value that does not exist (rather than returning NULL)
# the point of this is to throw errors early if the data changes.
`$.checked_list` <- function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab <- deparse(substitute(y))
  }
  if(!ylab %in% names(x)) {
    stop("The value `",ylab,"` is not a valid entry") # for ",xlab)
  }
  NextMethod()
}


## Summarisation helpers ----

#' Bind_rows works until there are factors with a set of different levels.
#'
#' @param ... a list of dataframes
#'
#' @return the union of those dataframes. Factor levels are combined with levels
#' @export
#'
#' @examples
#' bind_rows_with_factors(iris,diamonds)
bind_rows_with_factors <- function(...) {
  # Identify all factors, and all their levels and
  dots = rlang::list2(...)
  factors = unique(unlist(purrr::map(dots, ~ .x %>% ungroup() %>% select(where(is.factor)) %>% colnames())))
  factorLevels = bind_rows(purrr::map(dots, ~ purrr::map(.x %>% ungroup() %>% select(where(is.factor)), ~ .x %>% levels()) %>% enframe() %>% unnest(c(value))))
  # convert factors to character, bind dataframes, convert characters back to factors
  dots2 = purrr::map(dots, ~ .x %>% mutate(across(where(is.factor),as.character)))
  out = bind_rows(dots2)
  for (col in factors) {
    # combine the factors. This will tend to keep the order of the levels the first time a factor is encountered
    l = factorLevels %>% filter(name==col) %>% pull(value) %>% unique()
    out[[col]] = factor(out[[col]],levels = l)
  }
  return(out)
}


#' Summarise and include a total row, or a row including the summary for the whole group, into a factor list.
#'
#' @param .data a dataframe
#' @param ... the summarisation specification
#' @param .groups what to do with the grouping after summarisation (same as dplyr::summarise)
#' @param .total name of the total row which will be added into a factor list.
#' @param .total_first should the total be before or after the groups
#'
#' @return a dataframe with the additional
#' @export
#'
#' @examples
#' diamonds %>% group_by(color,cut) %>%
#'    summarise_with_totals(mpg = sprintf("%1.1f \u00B1 %1.1f", mean(price), sd(price)), .total = "Overall")
summarise_with_totals = function(.data, ..., .groups = NULL, .total="Total", .total_first = FALSE) {
  grps = .data %>% groups()
  last_col = (tail(grps,1)[[1]])
  first_level = .data %>% summarise(...,.groups=.groups)
  grps_out = first_level %>% groups()
  totals = .data %>% ungroup() %>% group_by(!!!(grps %>% head(-1))) %>% summarise(...,.groups=.groups)

  if (.data %>% pull(!!last_col) %>% is.factor()) {
    if (.data %>% pull(!!last_col) %>% is.ordered()) {
      totals = totals %>% mutate(!!last_col := .total %>% ordered())
    } else {
      totals = totals %>% mutate(!!last_col := .total %>% factor())
    }
    if (.total_first) {
      out = bind_rows_with_factors(totals, first_level)
    } else {
      out = bind_rows_with_factors(first_level, totals)
    }
  } else {
    totals = totals %>% mutate(!!last_col := .total %>% factor())
    first_level = first_level %>% mutate(!!last_col := as.character(!!last_col))
    if (.total_first) {
      out = bind_rows(totals,first_level)
    } else {
      out = bind_rows(first_level,totals)
    }
  }
  return(out %>% group_by(!!!grps_out))
}

#' Create a new data frame including duplicate rows where the rows fulfil a potentially overlapping set of conditions
#'
#' @param .data a data frame
#' @param ... a set of predicates specified like case_whens syntax, such as mpg < 5 ~ "gas guzzlers"
#' @param .colname the name of the new group
#'
#' @return a new dataframe containing the overlapping groups.
#' @export
#'
#' @examples
#' iris %>% group_by(Species) %>% intersecting_group_by(
#'   Sepal.Length > mean(Sepal.Length) ~ "Long",
#'   Sepal.Width > mean(Sepal.Width) ~ "Wide"
#' )
intersecting_group_by = function(.data, ..., .colname = "group") {
  .colname = ensym(.colname)
  grps = .data %>% groups()
  dots = rlang::list2(...)
  bind_rows(lapply(dots, function(form) {
    value = rlang::f_rhs(form)
    predicate = rlang::f_lhs(form)
    .data %>% filter(!!predicate) %>% mutate(!!.colname := value)
  })) %>% group_by(!!!grps, !!.colname)
}

# .maybe = function(expr) {
#   expr = enexpr(expr)
#   browser()
#   #data = .search_call_stack()
#   tryCatch(rlang::eval_tidy(expr,tidyselect::peek_data()), error = function(e) {
#     warning(e$message)
#     NA}
#   )
# }

# TODO:
# fct_set_na = function(x, predicateExpr) {
#   predicateExpr = enexpr(predicateExpr)
#   tmp = ifelse(!!predicateExpr, NA, as.character(x))
#   factor(tmp, levels=levels(x), ordered = is.ordered(x))
# }
