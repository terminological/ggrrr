## Dataset description helpers ----


#' Get a value set list of a dataframe
#'
#' This function examines a dataframe and returns a list of the columns with sub-lists as all the options for factors.
#' This provides programmatic access (and autocomplete) to the values available in a dataframe, and throws and early
#' error if we try and access data by a variable that does not exist.
#'
#' @param df a dataframe to examine
#'
#' @return a list of lists with the column name and the factor levels as list, as a `checked list`.
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

# This is a sub class of list access operator that throws an error if you attempt to access a value that does not exist (rather than returning NULL)
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
