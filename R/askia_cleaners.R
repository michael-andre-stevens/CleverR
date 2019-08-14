#' Cleanup Askia output
#'
#' @param x a variable or data.frame
#' @param ... further arguments passed to or from other methods
#' @param df a data.frame
#' @param df2 a second data.frame
#' @param pattern pattern in the names of the variables that form a 'multi'
#' @param label label for the 'multi' (often the question that was asked)
#' @details
#' \code{askia_clean_label} removes html from a variable's label attribute
#'
#' \code{askia_clean_levels} removes html from factor levels
#'
#' \code{askia_as_integer} coerces a variable to integer while keeping the label attribute
#'
#' \code{askia_as_ordered} coerces a variable to an ordered factor while keeping the label attribute
#'
#' \code{askia_as_reverse_ordered} coerces a variable to a reverse-ordered factor while keeping the label attribute
#'
#' \code{askia_as_likert} coerces a factor to a likert (an integer) if possible (i.e. if all but the highest and lowest levels are integer values).
#'
#' \code{askia_reorder_multi} converts 'multi' variables so that they indicate presence instead of ordering of the alternatives
#'
#' \code{askia_replace_multi} replaces 'multi' variables in \code{df} with their recoded version in \code{df2}
#'
#'
#' @return the cleaned variable(s)
#'
#' @name askia_cleaners
NULL
#> NULL


#' @rdname askia_cleaners
#' @export
askia_clean_label <- function(x) {

  labelstring <- attr(x, "label")
  labelstring <- gsub("<.*?>", "", labelstring)
  labelstring <- gsub("[\t\r\n]", "", labelstring)
  attr(x, "label") <- labelstring
  x
}


#' @rdname askia_cleaners
#' @export
askia_clean_levels <- function(x) {

  if (is.factor(x)) {
    levelstring <- levels(x)
    levelstring <- gsub("<.*?>", "", levelstring)
    levelstring <- gsub("[\t\r\n]", "", levelstring)
    levels(x) <- levelstring
  }
  x
}

#' @rdname askia_cleaners
#' @export
askia_as_integer <- function(x) {

  label <- attr(x, "label")
  x <- as.integer(x)
  attr(x, "label") <- label
  x
}


#' @rdname askia_cleaners
#' @export
askia_as_ordered <- function(x, ...) {

  label <- attr(x, "label")
  x <- ordered(x)
  attr(x, "label") <- label
  x
}

#' @rdname askia_cleaners
#' @export
askia_as_reverse_ordered <- function(x) {

  label <- attr(x, "label")
  x <- ordered(x, levels=rev(levels(x)))
  attr(x, "label") <- label
  x
}


#' @rdname askia_cleaners
#' @export
askia_as_likert <- function(x) {

  if(is.factor(x)) {
    is_likert <- as.character(as.numeric(levels(x))) == levels(x)
    is_likert <- is_likert[2:(length(is_likert)-1)]
    is_likert <- all(is_likert)

    if(isTRUE(is_likert)) {
      x <- askia_as_integer(x)
    }
  }
  x
}


#' @rdname askia_cleaners
#' @export
askia_reorder_multi <- function(df, pattern, label=NULL) {

  tmp <- df %>% dplyr::select(dplyr::matches(pattern))
  tmplvl <- levels(tmp[,1]  %>% dplyr::pull)
  tmp <- tmp %>% dplyr::mutate_all(as.numeric)

  testxy <- function(x, y) y %in% x

  out <- data.frame(matrix(NA, nrow=nrow(tmp), ncol=ncol(tmp)))
  names(out) <- names(tmp)
  for(i in 1:ncol(tmp)) {

    #out[,i] <- as.factor(as.numeric(apply(tmp, 1, testxy, i)))
    out[,i] <- apply(tmp, 1, testxy, i)

    if(is.null(label)) {
      attr(out[,i], "label") <- tmplvl[i]
    } else {
      attr(out[,i], "label") <- paste(label, ":", tmplvl[i])
    }
  }

  out
}


#' @rdname askia_cleaners
#' @export
askia_replace_multi <- function(df, df2) {
  for (i in intersect(names(df), names(df2))) {
    df[[i]] <- df2[[i]]
  }
  df
}


