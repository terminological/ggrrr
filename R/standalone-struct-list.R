# ---
# repo: terminological/ggrrr
# file: standalone-struct-list.R
# last-updated: 2024-03-24
# license: https://unlicense.org
# imports:
#    - knitr
#    - pillar
#    - purrr
#    - rlang
#    - tibble
#    - utils
# ---

#' Create a structure.
#'
#' @param ... a set of named values to use in the structure. or an unnamed list
#'   of `struct`s of the same type
#' @param .class the class of the nested list
#' @param .attr attributes to assign to the structure
#'
#' @return Usually a length one `struct_list` with a single `struct` item with class `.class`
#'   and attributes `.attr`. If a list of `struct`s is provided then it will be
#'   longer than 1.
#' @export
#' @keywords internal
#' @concept structures
#'
#' @examples
#' x = struct(a=1,b=2,c=1:3,.class="test")
#' format.test = function(x,...) {sprintf("\"%d/%d [%d-%d]\"", x$a, x$b, min(x$c), max(x$c))}
#' x # is a struct_list of length 1
#' x[[1]] # is a struct of length 1
#'
#' # e = struct(.class="test")
#' try(struct())
#'
#' x =
#'   struct(
#'     struct(a=1,b=2,c=1:3,.class="test"),
#'     struct(a=2,b=3,c=4:5,.class="test")
#'   )
#'
#' tmp = lapply(1:10, function(x) struct(a=x,b=3,c=4:5,.class="test"))
#' struct(tmp)
#'
struct = function(..., .class = NULL, .attr = list()) {
  tmp = rlang::list2(...)

  # tmp is a list. It may contain a set of named parameters to create a struct
  # if it is named them we are creating a new struct

  if (!(is.null(names(tmp)) || all(names(tmp)==""))) {
    # This is a new struct definition
    # This will also catch tmp as empty list
    if (is.null(.class)) stop("`.class` parameter must be specified for any new structures",call. = FALSE)
    # This must use the list format of the inputs:
    tmp = as.struct(tmp, .class)
    attributes(tmp) = c(attributes(tmp),.attr)
    return(as.struct_list(tmp))
  }

  if (!all(names(tmp)=="")) {
    # There is a mix of names and unnamed parameters in the ... parameters
    # this is an error
    stop("`struct` parameters must be either all named or all unnamed structs or struct_lists",call. = FALSE)
  }

  # tmp is at least one level of list due to rlang::list2
  tmp = unlist(tmp,recursive = FALSE)

  # We are assuming that the tmp list contains struct_lists or structs to be unified
  if (!.list_is(tmp, function(x) {is.struct_list(x) || is.struct(x)} )) {
    stop("unnamed parameters to `struct` must be either structs or struct_lists")
  }

  return(as.struct_list(tmp))

}

# struct S3 class ----

#' @export
#' @keywords internal
#' @concept structures
as.character.struct = function(x,...) {
  return(format(x,...))
}

#' @export
#' @concept structures
print.struct = function(x,...) {
  cat(suppressWarnings(format(x,...)))
}

#' @exportS3Method knitr::knit_print
#' @concept structures
knit_print.struct = function (x, ...) {
  structure(format(x), class = "knit_asis")
}

#' @export
#' @concept structures
length.struct = function(x,...) {
  return(1)
}

#' @exportS3Method pillar::type_sum struct
#' @concept structures
type_sum.struct = function(x,...) {
  abbreviate(.subclass(x),3)
}

#' @describeIn struct Check is a `struct`
#' @export
#' @concept structures
is.struct = function(x, .class = NULL) {
  if (!inherits(x,"struct")) return(FALSE)
  if (!is.null(.class)) return(.subclass(x) == .class)
  return(TRUE)
}

#' @describeIn struct Cast to a `struct`
#' @export
#' @concept structures
as.struct = function(x, .class) {
  class(x) = c(.class,"struct")
  return(x)
}

#' @describeIn struct Make `struct` into plain list
#' @export
#' @concept structures
as.list.struct = function(x,...) {
  list(x)
}

# @exportS3Method tibble::as_tibble struct
# as_tibble.struct = function(x,...) {
#
# }

# struct_list S3 class ----

#' Ensure `struct_list` is a flat list of `structs`
#'
#' Unlike `purrr::list_flatten` this is recursive for one reason
#' which is that a `struct_list` must only contain `structs`.
#'
#' @param x a potentially nested list of `struct_lists`.
#'
#' @return a flat `struct_list` of `structs`
#' @export
#' @concept structures
struct_flatten = function(x) {
  as.struct_list(x)
}

#' @export
#' @concept structures
length.struct_list = function(x,...) {
  return(length(unclass(x)))
}

#' @describeIn struct is a list of `structs`?
#' @export
#' @concept structures
is.struct_list = function(x, .class = NULL) {
  if (!inherits(x,"struct_list")) return(FALSE)
  if (!is.null(.class)) return(.subclass(x) == .class)
  return(TRUE)
}

# empty list always returns TRUE
.list_is = function(l, .f, ...) {
  .f = rlang::as_function(.f)
  return(all(sapply(l, .f, ...)))
}

# x is a list but not a struct. This means it is a plain list
# or a struct_list
.is_list_excl_struct = function(x) {
  if (!is.list(x)) return(FALSE)
  if (is.struct(x)) return(FALSE)
  return(TRUE)
}

# gets stated class information from struct or struct_lists, resolving
# recursively plain lists. e.g. list(struct, struct_list, list(struct, struct)) are all
# checked to be the same.
.subclass = function(x) {
  tmp = .subclass_r(x)
  if (length(tmp) == 0) stop("Empty plain list detected. No type information.",call. = FALSE)
  return(tmp)
}

.subclass_r = function(x) {
  if (is.struct(x)) return(utils::head(class(x),1))
  if (is.struct_list(x)) return(utils::tail(class(x),1))
  if (is.list(x)) {
    # this is recursive:
    tmp = unique(unlist(lapply(x, .subclass_r)))
    if (length(tmp) == 1) return(tmp)
    if (length(tmp) > 1) stop("All structs must be of the same type. Mixed list of structs detected: ",paste0(tmp,collapse=", "),call. = FALSE)
    return(tmp) # size zero / no class info.
  }
  # This is basically an error though:
  stop("Item is not a `struct_list`, a `struct` or a uniform list of `structs`.",call. = FALSE)
  # return(class(x))
}

.is_compatible = function(x,y) {
  return(.subclass(x)==.subclass(y))
}

.method_exists = function(.class, .method) {
  return(is.null(utils::getS3method(.method, .class, optional=TRUE)))
  #return(.method %in% methods(class=.class))
}


#' Cast to a list of structures
#'
#' TODO: testing this
#' empty input (must specify .class)
#' struct_list - identity
#' simple struct - wrapped
#' list of struct_lists is flattened
#' list of structs is wrapped
#' nested plain lists / struct_lists are flattened
#'
#' @param x a list
#' @param .class the type of structures in the list
#' @return a structured list
#' @export
#' @concept structures
as.struct_list = function(x, .class=NULL) {

  if (length(unlist(x)) == 0) {
    # .class may be asserted when creating zero size / NULL `struct`s
    if (is.null(.class)) .class = .subclass(x)
    # if x is empty struct_list .subclass will work
    # otherwise this will throw an error.


    return(structure(list(),class=c("struct_list","list",.class)))
  }
  if (is.struct(x)) return(structure(list(x),class=c("struct_list","list",.subclass(x))))
  if (is.list(x)) {
    # x is a list or struct_list (but cannot be a single struct at this point)
    while (any(sapply(x, .is_list_excl_struct))) {
      # if there are any nested struct_lists or plain lists we will collapse
      # them as a struct_list must be a list of structs without hierarchy.
      # this will also have the side effect of wrapping plain structs
      x = lapply(x, as.list)
      x = unlist(x, recursive = FALSE)
    }
    # rely on .subclass to detect any invalid non-structs or mixed struct types
    # and throw an error.
    return(structure(x,class=c("struct_list","list",.subclass(x))))
  }

  stop("Not convertible to a struct_list, x is not `struct_list`, a `struct` or a uniform list of `structs`",call. = FALSE)
}

#' @export
#' @concept structures
as.list.struct_list = function(x,...) {
  unclass(x)
}

#' @export
#' @concept structures
format.struct_list = function(x, ...) {
  unlist(lapply(x,format))
}

#' @export
#' @concept structures
print.struct_list = function(x,...) {
  if (length(x) == 0) {
    cat(.subclass(x),"()",sep = "")
  } else {
    cat(.subclass(x),"(",paste0(names(x[[1]]), collapse=", "),")\n",sep = "")
  }
  cat(suppressWarnings(format.struct_list(x,...)))
}

# @exportS3Method knitr::knit_print
# knit_print.struct_list = function (x, meta = NULL, cacheable = NA) {
#   structure(format(x), class = "knit_asis", knit_meta = meta, knit_cacheable = cacheable)
# }

#' @export
#' @concept structures
as.character.struct_list = function(x,...) {
  if (length(x) == 0) cat(.subclass(x),"()",sep = "")
  format.struct_list(x,...)
}

#' @exportS3Method pillar::type_sum struct_list
#' @concept structures
type_sum.struct_list = function(x,...) {
  I(sprintf("<%s[]>", abbreviate(.subclass(x),3,named = FALSE)))
}

#' @exportS3Method pillar::pillar_shaft
#' @concept structures
pillar_shaft.struct_list <- function(x, ...) {
  out <- format.struct_list(x, ...)
  pillar::new_pillar_shaft_simple(out, align = "right")
}


#' Manipulate structured lists
#'
#' These functions allow generic list behaviour.
#'
#' @return a `struct_list` with all the items
#'
#' @examples
#'
#' x = struct(a=1,b=2,c=1:3,.class="test")
#' y = struct(a=4,b=5,c=1:3,.class="test")
#' z= tibble::tibble(a= 1:10, b=rep(c(x,y),5))
#'
#' z$b
#'
#' c(x,y)
#' c(rep(x,5),y)
#' class(c(rep(x,5),rep(y,5))[[1]])
#'
#' as.struct_list(list(x,y))
#'
#' #' x = struct(a=1,b=2,c=1:3,.class="test")
#' class(rep(c(x,y),5)[[1]]) == "test"
#' class(rep(x,5))
#'
#' a = (rep(c(x,y),5))
#' a[[1]] = y
#' a
#'
#' @concept structures
#' @name subset-struct-list
NULL

#' @describeIn subset-struct-list Repeat a `struct_list`
#' @param ... some of `struct_list` and `struct` or list of `struct`s
#' @export
c.struct_list = function(...) {
  dots = rlang::list2(...)
  if (is.struct_list(dots)) return(dots)
  if (length(dots) == 1) return(as.struct_list(dots))
  # remove empty items
  dots = dots[sapply(dots,length)>0]
  # make sure all list entries are a struct list (tmp is list of struct_list)
  tmp = lapply(dots, as.struct_list)
  # convert to plain list of lists
  tmp = lapply(tmp, as.list.struct_list)
  # collapse one level
  tmp = unlist(tmp,recursive = FALSE)
  # convert to struct_list. this should throw an error if types are mixed.
  return(as.struct_list(tmp))
}

#' @describeIn subset-struct-list Repeat a `struct_list`
#' @param x a `struct_list`
#' @param ... generic support
#' @export
rep.struct_list = function(x, ...) {
  tmp = NextMethod()
  # if (length(x) != 1) tmp = unlist(tmp,recursive = FALSE)
  return(.clone_struct_list(tmp,x))
}

# Subsetting functions ----

.clone_struct_list = function(new,old) {
  attributes(new) = attributes(old)
  return(new)
}

#' @describeIn subset-struct-list Subset a `struct_list`
#' @param y item to retrieve
#' @export
`$.struct_list` = function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab = deparse(substitute(y))
  }
  if (length(x)==1) return(x[[1]][[ylab]])
  return(sapply(seq_along(x), function(i) x[[i]][[ylab]], USE.NAMES = FALSE))
}


#' @describeIn subset-struct-list Subset a `struct_list`
#' @export
`[.struct_list` = function(x, ...) {
  y = `[`(unclass(x),...)
  return(.clone_struct_list(y, x))
}

#' @describeIn subset-struct-list Assign a subset to a `struct_list`
#' @param x a `struct_list`
#' @param ... generic support
#' @param value the value
#' @export
`[<-.struct_list` = function(x,...,value) {
  if (!is.struct_list(value) || !.is_compatible(x,value)) {
    stop("cannot add a list of `",.subclass(value),"` to a list of `",.subclass(x),"`")
  }
  y = `[<-`(unclass(x),...,value)
  return(.clone_struct_list(y, x))
}

#' @describeIn subset-struct-list get a value from a `struct_list`
#' @param x a `struct_list`
#' @param ... generic support
#' @export
`[[.struct_list` = function(x,...) {
  y = `[[`(unclass(x),...)
  return(y)
}

#' @describeIn subset-struct-list set a single value in a `struct_list`
#' @param x a `struct_list`
#' @param ... generic support
#' @param value the value
#' @export
`[[<-.struct_list` = function(x,...,value) {
  if (is.struct_list(value) && length(value) == 1) value = value[[1]]
  if (!is.struct(value, .class=.subclass(x))) stop("cannot add a `",.subclass(value),"` to a struct_list of `",.subclass(x),"`")
  y = `[[<-`(unclass(x),...,value)
  return(.clone_struct_list(y, x))
}

## Mathematical functions ----
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/groupGeneric.html


#' Binary operations
#'
#' @keywords internal
#' @export
#' @concept structures
#' @examples
#'
#' as.double.test = function(x) {
#'   return(x$a)
#' }
#'
#' `+.test` = function(e1,e2) {
#'   e1$a = e1$a + as.numeric(e2)
#'   return(e1)
#' }
#'
#' x = struct(a=1,b=2,c=1:3,.class="test")
#'
#' # TODO: needs debugging
#' # x+3
#' # x[[1]]+3
#' # rep(x,5)+1:5
#' # x[[1]]+x[[1]]
#'
`Ops.struct_list` = function(e1,e2) {
  if (nargs() == 2L) {
    # BINARY
    if (length(e1)==length(e2)) {
      out = list()
      for (i in seq_along(e1)) {
        out = c(out, list(get(.Generic)(e1[[i]],e2[[i]])))
      }
      return(.clone_struct_list(out, e1))
    } else {
      if (!length(e2) >= 2) {
        stop("incompatible sizes in binary operation with `",.subclass(e1),"`")
      } else {
        for (i in seq_along(e1)) {
          out = c(out, list(get(.Generic)(e1[[i]],e2)))
        }
        return(.clone_struct_list(out, e1))
      }
    }
  } else {
    # UNARY
    out = lapply(e1, get(.Generic))
    return(.clone_struct_list(out, e1))
  }
}

#' Maths operations
#'
#' abs, sign, sqrt, floor, ceiling, trunc, round, signif
#' exp, log, expm1, log1p,
#' cos, sin, tan, cospi, sinpi, tanpi, acos, asin, atan, cosh, sinh, tanh, acosh, asinh, atanh
#' lgamma, gamma, digamma, trigamma
#' cumsum, cumprod, cummax, cummin
#'
#' @keywords internal
#' @export
#' @concept structures
#' @examples
#' # example code
#'
#' abs.test = function(x,...) {
#'   x$a = abs(x$a)
#'   return(x)
#' }
#'
#' x = struct(a=-1,b=2,c=1:3,.class="test")
#' y = struct(a=4,b=5,c=1:3,.class="test")
#'
#' abs.test(x[[1]])
#'
#' # TODO: further testing
#' # abs(rep(c(x,y),5))
#'
`Math.struct_list` = function(x,...) {
  y = lapply(unclass(x), get(.Generic), ...)
  return(.clone_struct_list(y, x))
}

#' Summary operations
#'
#' all, any, sum, prod, min, max, range
#'
#' @keywords internal
#' @concept structures
#' @export
#' @examples
#'
#' sum.test = function(..., na.rm=TRUE) {
#'   # must use rlang::list2
#'   l = rlang::list2(...)
#'   return(purrr::reduce(l, `+.test`))
#' }
#'
#' map_struct(1:10, ~ struct(a=.x,b=2,c=1:3,.class="test"))
#'
#' min.test = function(..., na.rm=TRUE) {
#'   l = rlang::list2(...)
#'
#' }
#'
#' # class(sum(z$b))
#' # sum(z$b, z$b)
#'
`Summary.struct_list` = function(..., na.rm=FALSE) {
  tmp = rlang::list2(...)
  if (!is.struct_list(tmp)) tmp = unlist(tmp,recursive = FALSE)
  # this will call the `sum.test` function if tmp is a list of `test` class
  # the `sum.test(...)` dots will be a plain list of `test` classes
  y = do.call(get(.Generic), tmp)
  return(as.struct_list(y))
}


#' @param .x a `struct_list`
#' @param .f a function to apply to each structure
#' @param ... additional parameters to  `.f` but anonymous function preferred
#' @param .progress display a progress bar (logical or string name)
#' @seealso [purrr::map()]
#'
#' @inherit purrr::map title description details
#' @returns a `struct_list`
#' @export
#' @concept structures
map_struct = function(.x, .f, ..., .progress=FALSE) {
  # This will flatten any nested struct_lists. This is good as .f may return a
  # single struct or more likely a 1 element struct_list.
  return(purrr::map(.x, .f, ..., .progress=.progress) %>% as.struct_list())
}

#' @param .x a `struct_list`
#' @param .y a list or vector the same length as `.x`
#' @param .f a function to apply to each `.x`, `.y` pair
#' @param ... additional parameters to  `.f` but anonymous function preferred
#' @param .progress display a progress bar (logical or string name)
#'
#' @inherit purrr::map2 title description details
#' @returns a `struct_list`
#' @export
#' @concept structures
map2_struct = function(.x, .y, .f, ..., .progress = FALSE) {
  return(purrr::map2(.x, .y, .f, ..., .progress=.progress) %>% as.struct_list())
}

#' @param .l A `struct_list`.
#' @param .f a function to apply to each `.l` item
#' @param ... additional parameters to  `.f` but anonymous function preferred
#' @param .progress display a progress bar (logical or string name)
#' @inherit purrr::pmap title description details
#' @returns a `struct_list`
#' @export
#' @concept structures
pmap_struct = function(.l, .f, ..., .progress = FALSE) {
  return(
    purrr::map(as.list.struct_list(.l), \(item) do.call(.f, item), ..., .progress = .progress) %>% as.struct_list()
  )
}
