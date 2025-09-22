# ---
# repo: terminological/ggrrr
# file: standalone-df-list-df.R
# last-updated: '2025-09-22'
# license: https://unlicense.org
# imports:
# - dplyr
# - purrr
# - testthat
# - tidyr
# ---

#' Convert a nested dataframe to a multilevel list
#'
#' @param df a nested dataframe
#'
#' @returns a list of lists
#' @keywords internal
#'
#' @unit
#'
#' iris_list = .df_to_list_of_lists(iris)
#'
#' testthat::expect_equal(
#'   iris_list[[1]]$Species,
#'   iris$Species[[1]]
#' )
#'
#' mtcars_nest = mtcars %>%
#'   dplyr::mutate(name = rownames(.)) %>%
#'   tidyr::nest(details = -c(cyl,gear))
#'
#' mtcars_list = mtcars_nest %>% .df_to_list_of_lists()
#'
#' testthat::expect_equal(
#'   mtcars_list[[1]]$details[[1]]$name,
#'   mtcars_nest$details[[1]]$name[[1]]
#' )
#'
.df_to_list_of_lists = function(df) {
  if (nrow(df) == 1) {
    list = as.list(df)
    for (i in seq_along(list)) {
      if (is.data.frame(list[[i]])) {
        # translate nested df into lists
        list[[i]] = .df_to_list_of_lists(list[[i]])
      } else if (is.list(list[[i]])) {
        # nested dataframes are more likely a length 1 list containing a
        # data frame
        if (length(list[[i]]) && is.data.frame(list[[i]][[1]])) {
          list[[i]] = .df_to_list_of_lists(list[[i]][[1]])
        }
      }
    }
    return(list)
  }
  df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    tidyr::nest(.row = -.id) %>%
    dplyr::mutate(.row = purrr::map(.row, .df_to_list_of_lists)) %>%
    dplyr::pull(.row)
}

#' Convert a multilevel list to a nested dataframe
#'
#' @param list a multilevel list
#'
#' @returns a dataframe with each sublist nested as a dataframe
#' @keywords internal
#'
#' @unit
#' iris_list = .df_to_list_of_lists(iris)
#' iris2 = .list_of_lists_to_df(iris_list)
#'
#' testthat::expect_equal(iris, as.data.frame(iris2))
#'
#' mtcars_nest = mtcars %>%
#'   dplyr::mutate(name = rownames(.)) %>%
#'   tidyr::nest(details = -c(cyl,gear))
#'
#' mtcars_list = mtcars_nest %>% .df_to_list_of_lists()
#'
#' mtcars_nest2 = mtcars_list %>% .list_of_lists_to_df()
#'
#' testthat::expect_equal(
#'   mtcars_nest2$details[[2]],
#'   mtcars_nest$details[[2]]
#' )
#'
.list_of_lists_to_df = function(list) {
  list_lengths = sapply(list, length)
  is_unnamed = is.null(names(list))

  # Any unnamed list of any length is a set of rows
  if (is_unnamed) {
    return(dplyr::bind_rows(
      lapply(seq_along(list), function(i) .list_of_lists_to_df(list[[i]]))
    ))
  }
  # Named lists are a single row
  # These might have nested lists:
  for (i in seq_along(list)) {
    if (is.data.frame(list[[i]])) {
      # do nothing
    } else if (is.list(list[[i]])) {
      # nested list column
      # the nested list columns can be made into dataframes
      # these have to be wrapped in a length 1 list to look like a nested df
      list[[i]] = list(.list_of_lists_to_df(list[[i]]))
    } else if (length(list[[i]]) > 1) {
      # this is a nested vector we turn this into a nested purrr style list
      # to prevent recycling
      list[[i]] = list(list[[i]])
    }
  }
  # every entry is now single length:
  return(dplyr::as_tibble(list))
}
