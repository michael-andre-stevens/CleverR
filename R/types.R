#' Print a clever_tbl object
#'
#' @param x a clever_tbl
#' @param maxwidth maximum width of a field. Defaults to 80
#' @param ... further arguments passed to or from other methods
#'
#' @return returns the clever_tbl invisibly
#' @export
#'
#' @examples mtcars %>% as_clever_tbl() %>% print()
print.clever_tbl <- function(x, maxwidth=80, ...) {

  clever_trunc <- function(str, mw=maxwidth) {
    str <- stringr::str_trunc(str, mw)
  }

  x %>%
    dplyr::mutate_if(is.character, clever_trunc) %>%
    data.frame() %>%
    print(right=FALSE, row.names = FALSE, ...)
}


#' Coerce a data.frame into a clever_tbl.
#'
#' as_clever_tbl turns a data.frame into a clever_tbl, a data frame with class clever_tbl.
#'
#' @param x a data.frame
#'
#' @return a clever_tbl
#' @export
#'
#' @examples mtcars %>% as_clever_tbl()
as_clever_tbl <- function(x) {
  class(x) <- c("clever_tbl", class(x))
  x
}


###############################################################################

#' Type checks
#'
#' @param x a variable
#' @param min_x minimum value of a possible Likert scale. Defaults to 0.
#' @param max_x maximum value of a possible Likert scale. Defaults to 10.

#' @details
#' \code{is_unordered} checks if an object is an ordered factor
#'
#' \code{is_likert} checks if an object is a Likert rating scale. If an object has integer values in a limited range it is considered to be a Likert scale.
#'
#' \code{is_character_factor_logical_or_likert} checks if a vector is a character, factor, a logical or Likert scale.
#'
#' \code{is_numeric_logical_or_ordered} checks if a vector is numeric, logical or ordered
#'
#' \code{is_numeric_logical_ordered_or_date} checks if a vector is a numeric, logical, ordered or a date
#'
#' @return a logical
#'
#' @name type_checks
#' @keywords internal
NULL
#> NULL


#' @rdname type_checks
#' @keywords internal
is_unordered <- function(x) {
  is.factor(x) && !is.ordered(x)
}

#' @rdname type_checks
#' @keywords internal
is_likert <- function(x, min_x=0, max_x=10) {
  is.integer(x) && min(x, na.rm=TRUE) >= min_x && max(x, na.rm=TRUE) <= max_x
}


#' @rdname type_checks
#' @keywords internal
is_character_factor_logical_or_likert <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x) || is_likert(x)
}


#' @rdname type_checks
#' @keywords internal
is_numeric_logical_or_ordered <- function(x) {
  is.numeric(x) || is.logical(x) || is.ordered(x)
}


#' @rdname type_checks
#' @keywords internal
is_numeric_logical_ordered_or_date <- function(x) {
  is.numeric(x) || is.logical(x) || is.ordered(x) || lubridate::is.Date(x)
}



###############################################################################

#' Type conversions
#'
#' @param x a variable
#' @param max_distinct_values maximum number of factor levels to be used when converting a character vector. Defaults to 11.
#'
#' @details
#' \code{likert2factor_fnc} converts a likert scale to an ordered factor
#'
#' \code{likert2revfactor_fnc} converts a likert scale to a reverse-ordered factor
#'
#' \code{string2factor_fnc} converts a character vector to a factor, possibly lumping smaller categories together
#'
#' \code{logical2factor_fnc} converts a logical vector to an ordered factor
#'
#' \code{logical2revfactor_fnc} converts a logical vector to a reverse-ordered factor
#' @name type_conversion
#' @keywords internal
NULL
#> NULL


#' @rdname type_conversion
#' @keywords internal
likert2factor_fnc <- function(x) {
  x %>% ordered(levels=min(x, na.rm=TRUE):max(x, na.rm=TRUE))
}


#' @rdname type_conversion
#' @keywords internal
likert2revfactor_fnc <- function(x) {
  x %>% ordered(levels=max(x, na.rm=TRUE):min(x, na.rm=TRUE))
}


#' @rdname type_conversion
#' @keywords internal
string2factor_fnc <- function(x, max_distinct_values=11) {
  factor(x) %>% forcats::fct_lump(max_distinct_values, other_level = "(Other)", ties.method="random")
}


#' @rdname type_conversion
#' @keywords internal
logical2factor_fnc <- function(x) {
  factor(x) %>% ordered(levels=c(FALSE, TRUE))
}


#' @rdname type_conversion
#' @keywords internal
logical2revfactor_fnc <- function(x) {
  factor(x) %>% ordered(levels=c(TRUE, FALSE))
}



###############################################################################
#

#' Small fixes to variables
#'
#' @param x a variable
#' @param max_distinct_values maximum number of factor levels to be used.

#'
#' @details
#' \code{fix_order} fixes the levels of a factor in the order of appearance
#'
#' \code{na_to_zero} replaces missing values with 0
#'
#' \code{fct_lump_fnc} lumps together least common levels of a factor
#'
#' @name small_fixes
#' @keywords internal
NULL
#> NULL


#' @rdname small_fixes
#' @keywords internal
fix_order <- function(x) {
  ordered(x, levels=unique(x))
}

#' @rdname small_fixes
#' @keywords internal
na_to_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}



#' @rdname small_fixes
#' @keywords internal
fct_lump_fnc <- function(x, max_distinct_values=11) {
  x %>% forcats::fct_lump(max_distinct_values, other_level = "(Other)", ties.method="random")
}


