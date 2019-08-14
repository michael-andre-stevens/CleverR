###############################################################################

#' Compute the size of a data.frame
#'
#' @param x a data.frame
#'
#' @return a tibble with the number of observations and the number of variables (statistic - value)
#' @export
#'
#' @examples datasets::mtcars %>% compute_size()
#' @importFrom rlang .data
#' @importFrom rlang ":="
compute_size <- function(x) {

  n_rows <- x %>% nrow()
  n_cols <- x %>% ncol()

  data.frame(
    statistic=c("observations", "variables"),
    value=c(n_rows, n_cols),
    stringsAsFactors=FALSE) %>%
    tibble::as_tibble()  %>%
    as_clever_tbl()
}


#' get the names of the variables in a data.frame
#'
#' @param x a data.frame
#'
#' @return a tibble with the names of the variables in x (variable)
#' @keywords internal
name_func <- function(x) {

  names(x) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(variable=.data$value)
}


#' Compute a property or univariate statistic for all variables (columns) in a dataframe
#'
#' @param x a tibble
#' @param fun the function that computes the property or statistic
#' @param newname the name of the variable that will hold the result. Defaults to 'descriptive'.
#'
#' @return a tibble with the names of the variables in x (variable) and the statistic that was computed (newname)
#' @keywords internal
column_func <- function(x, fun, newname="descriptive") {
  x %>%
    dplyr::ungroup() %>%
    dplyr::summarize_all(fun) %>%
    tidyr::gather('variable', 'value') %>%
    dplyr::rename(!!newname := .data$value)
}

###############################################################################

#' Helper functions that compute properties of a variable
#'
#' @param x a variable
#' @details
#' \code{class_func} returns the class of a variable
#'
#' \code{levels_func} returns the levels of a factor
#'
#' \code{labels_func} returns the label of a variable
#'
#' \code{observed_func} returns the number of observed values in a variable
#'
#' \code{missing_func} returns the number of missing values in a variable
#'
#' \code{distinct_func} returns the number of distinct values in a variable
#'
#' @name properties
#' @keywords internal
NULL
#> NULL



#' @rdname properties
#' @keywords internal
class_func <- function(x) toString(class(x))

#' @rdname properties
#' @keywords internal
levels_func <-  function(x) toString(levels(x))

#' @rdname properties
#' @keywords internal
labels_func <- function(x) toString(attr(x, "label"))

#' @rdname properties
#' @keywords internal
observed_func <- function (x) sum(!is.na(x))

#' @rdname properties
#' @keywords internal
missing_func <- function (x) sum(is.na(x))

#' @rdname properties
#' @keywords internal
distinct_func <- function (x) dplyr::n_distinct(x)


###############################################################################

#' Compute a set of properties for all variables (columns) in a dataframe
#'
#' @param x a tibble
#'
#' @return a tibble with variables in the rows and properties in the columns
#' @export
#'
#' @examples datasets::mtcars %>% compute_properties()
compute_properties <- function(x) {

  x_names <- x %>% name_func()
  x_class <- x  %>% column_func(class_func, "class")
  x_label <- x %>% column_func(labels_func, "label")
  x_levels <- x %>% column_func(levels_func, "levels")
  x_observed <- x %>% column_func(observed_func, "observed")
  x_missing <- x %>% column_func(missing_func, "missing")
  x_distinct <- x %>% column_func(distinct_func, "distinct")

  x_names %>%
    dplyr::full_join(x_class, by = "variable") %>%
    dplyr::full_join(x_label, by = "variable") %>%
    dplyr::full_join(x_levels, by = "variable") %>%
    dplyr::full_join(x_observed, by = "variable") %>%
    dplyr::full_join(x_missing, by = "variable") %>%
    dplyr::full_join(x_distinct, by = "variable") %>%
    as_clever_tbl()
}


###############################################################################

#' Helper functions that compute univariate summaries of a variable
#'
#' @param x a variable
#' @param prob probability in [0,1].
#' @details
#' \code{mean_func} and \code{mean_date_func} return the mean of a (date) variable
#'
#' \code{sd_func} and \code{sd_date_func} return the standard deviation of a (date) variable
#'
#' \code{quantile_func} and \code{quantile_date_func} return a quantile of a (date) variable
#'
#' \code{min_func} and \code{min_date_func} return the minimum of (date) variable
#'
#' \code{Q1_func} and \code{Q1_date_func} return the first quartile of (date) variable
#'
#' \code{median_func} and \code{median_date_func} return median of (date) variable

#' \code{Q3_func} and \code{Q3_date_func} return third quartile of (date) variable
#'
#' \code{max_func} and \code{max_date_func} return maximum of (date) variable
#'
#' @name descriptives
#' @keywords internal
NULL
#> NULL

#' @rdname descriptives
#' @keywords internal
mean_func <- function(x) ifelse (is.numeric(x), mean(x, na.rm=TRUE), as.numeric(NA))

#' @rdname descriptives
#' @keywords internalt
sd_func <- function(x) ifelse (is.numeric(x), stats::sd(x, na.rm=TRUE), as.numeric(NA))

#' @rdname descriptives
#' @keywords internal
quantile_func <- function(x, prob) {

  if (is.numeric(x)) {
    suppressWarnings(stats::quantile(x, prob, na.rm=TRUE, names=FALSE))
  } else {
    as.numeric(NA)
  }
}

#' @rdname descriptives
#' @keywords internal
min_func <- function(x) quantile_func(x, 0)

#' @rdname descriptives
#' @keywords internal
Q1_func <- function(x) quantile_func(x, 0.25)

#' @rdname descriptives
#' @keywords internal
median_func <- function(x) quantile_func(x, 0.5)

#' @rdname descriptives
#' @keywords internal
Q3_func <- function(x) quantile_func(x, 0.75)

#' @rdname descriptives
#' @keywords internal
max_func <- function(x) quantile_func(x, 1)

#' @rdname descriptives
#' @keywords internal
mean_date_func <- function(x) dplyr::if_else (lubridate::is.Date(x), mean(x, na.rm=TRUE), as.Date(NA))

#' @rdname descriptives
#' @keywords internal
sd_date_func <- function(x) dplyr::if_else (lubridate::is.Date(x), stats::sd(x, na.rm=TRUE), as.numeric(NA))

#' @rdname descriptives
#' @keywords internal
quantile_date_func <- function(x, prob) {
  if (lubridate::is.Date(x)) {
    x <- sort(x)
    idx <- round((length(x)-1)*prob)+1
    x[idx]
  } else {
    as.Date(NA)
  }
}

#' @rdname descriptives
#' @keywords internal
min_date_func <- function(x) quantile_date_func(x, 0)

#' @rdname descriptives
#' @keywords internal
Q1_date_func <- function(x) quantile_date_func(x, 0.25)

#' @rdname descriptives
#' @keywords internal
median_date_func <- function(x) quantile_date_func(x, 0.5)

#' @rdname descriptives
#' @keywords internal
Q3_date_func <- function(x) quantile_date_func(x, 0.75)

#' @rdname descriptives
#' @keywords internal
max_date_func <- function(x) quantile_date_func(x, 1)



###############################################################################

#' Compute a set of univariate statistics for all variables (columns) in a dataframe
#'
#' @param x a tibble
#' @details
#' \code{compute_descriptives} computes the statistics for numeric, logical and ordered variables
#'
#' \code{compute_descriptives_numeric} computes the statistics for numeric variables
#'
#' \code{compute_descriptives_logical} computes the statistics for logical variables
#'
#' \code{compute_descriptives_ordered} computes the statistics for ordered variables
#'
#' \code{compute_descriptives_date} computes the statistics for date variables
#'
#' @return a tibble with variables in the rows and univariate statistics in the columns
#' @export
#'
#' @examples datasets::mtcars %>% compute_descriptives()
compute_descriptives <- function(x) {

  x <- x %>% dplyr::select_if(is_numeric_logical_or_ordered)

  x_class <- x %>% column_func(class_func, "class")

  x <- x %>% dplyr::mutate_if(is.logical, as.numeric)
  x <- x %>% dplyr::mutate_if(is.ordered, as.numeric)

  x_mean <- x %>% column_func(mean_func, "mean")
  x_sd <- x %>% column_func(sd_func, "sd")
  x_min <- x %>% column_func(min_func, "min")
  x_Q1 <- x %>% column_func(Q1_func, "Q1")
  x_median <- x %>% column_func(median_func, "median")
  x_Q3 <- x %>% column_func(Q3_func, "Q3")
  x_max <- x %>% column_func(max_func, "max")

  x_class %>%
    dplyr::full_join(x_mean, by = "variable") %>%
    dplyr::full_join(x_sd, by = "variable") %>%
    dplyr::full_join(x_min, by = "variable") %>%
    dplyr::full_join(x_Q1, by = "variable") %>%
    dplyr::full_join(x_median, by = "variable") %>%
    dplyr::full_join(x_Q3, by = "variable") %>%
    dplyr::full_join(x_max, by = "variable") %>%
    as_clever_tbl()
}

#' @rdname compute_descriptives
#' @export
compute_descriptives_numeric <- function(x) {
  x %>% dplyr::select_if(is.numeric) %>% compute_descriptives()
}


#' @rdname compute_descriptives
#' @export
compute_descriptives_logical <- function(x) {
  x %>% dplyr::select_if(is.logical) %>% compute_descriptives()
}


#' @rdname compute_descriptives
#' @export
compute_descriptives_ordered <- function(x) {
  x %>% dplyr::select_if(is.ordered) %>% compute_descriptives()
}


#' @rdname compute_descriptives
#' @export
compute_descriptives_date <- function(x) {

  x <- x %>% dplyr::select_if(lubridate::is.Date)

  x_class <- x %>% column_func(class_func, "class")

  x_mean <- x %>% column_func(mean_date_func, "mean")
  x_sd <- x %>% column_func(sd_date_func, "sd")
  x_min <- x %>% column_func(min_date_func, "min")
  x_Q1 <- x %>% column_func(Q1_date_func, "Q1")
  x_median <- x %>% column_func(median_date_func, "median")
  x_Q3 <- x %>% column_func(Q3_date_func, "Q3")
  x_max <- x %>% column_func(max_date_func, "max")

  x_class %>%
    dplyr::full_join(x_mean, by = "variable") %>%
    dplyr::full_join(x_sd, by = "variable") %>%
    dplyr::full_join(x_min, by = "variable") %>%
    dplyr::full_join(x_Q1, by = "variable") %>%
    dplyr::full_join(x_median, by = "variable") %>%
    dplyr::full_join(x_Q3, by = "variable") %>%
    dplyr::full_join(x_max, by = "variable") %>%
    as_clever_tbl()
}






###############################################################################

#' Compute a set of frequency tables for all variables (columns) in a dataframe
#'
#' @param x a tibble
#' @param max_distinct_values maximum number of entries in the table. Defaults to 11.
#' @details
#' \code{compute_tables} computes the tables for numeric, logical and ordered variables
#'
#' \code{compute_tables_factor} computes the tables for factors
#'
#' \code{compute_tables_character} computes the tables for character variables
#'
#' \code{compute_tables_logical} computes the tables for logical variables
#'
#' \code{compute_tables_likert} computes the tables for Likert scales
#'
#' @return a tibble with variables in the rows and univariate statistics in the columns
#' @export
#'
#' @examples datasets::iris %>% compute_tables()

compute_tables <- function(x, max_distinct_values=11) {


  table_fnc <- function(x, variable) {

    table_fnc0 <- function(x, variable) {

      x %>%
        dplyr::group_by_at(variable) %>%
        dplyr::summarize(n=dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(pct=.data$n/sum(.data$n)*100)
    }

    xx <- x %>% dplyr::select(variable)

    out1 <- xx %>%
      dplyr::mutate_all(forcats::fct_explicit_na) %>%
      table_fnc0(variable) %>%
      dplyr::rename(pct_total=.data$pct) %>%
      dplyr::mutate_at(variable, as.character)

    out2 <- xx %>%
      dplyr::filter(!is.na(variable)) %>%
      table_fnc0(variable) %>%
      dplyr::rename(pct_observed=.data$pct) %>%
      dplyr::select(-.data$n) %>%
      dplyr::mutate_at(variable, as.character)

    out3 <-
      dplyr::left_join(out1, out2, by=variable) %>%
      dplyr::mutate(pct_observed = na_to_zero(.data$pct_observed)) %>%
      dplyr::mutate_at(variable, forcats::fct_explicit_na)

    if(!is.ordered(xx %>% dplyr::pull(variable))) {
      out3 <- out3 %>% dplyr::arrange(dplyr::desc(.data$pct_observed))
    }

    out3 <- out3 %>% dplyr::mutate_at(variable, fix_order)
    out3
  }


  x <- x %>% dplyr::select_if(is_character_factor_logical_or_likert)

  x <- x %>% dplyr::mutate_if(is.character, string2factor_fnc, max_distinct_values=max_distinct_values)
  x <- x %>% dplyr::mutate_if(is.factor, fct_lump_fnc, max_distinct_values=max_distinct_values)
  x <- x %>% dplyr::mutate_if(is.logical, logical2revfactor_fnc)
  x <- x %>% dplyr::mutate_if(is_likert, likert2revfactor_fnc)

  out <- vector("list", ncol(x))
  names(out) <- names(x)

  for (i in names(x)) {

    out[[i]] <-
      table_fnc(x, i) %>%
      dplyr::rename(value=i) %>%
      dplyr::mutate(value=as.character(.data$value)) %>%
      dplyr::mutate(variable=i) %>%
      dplyr::select(.data$variable, dplyr::everything())

  }

  out <- out %>% dplyr::bind_rows()

  out %>%
    as_clever_tbl()
}

#' @rdname compute_tables
compute_tables_factor <- function(x, max_distinct_values=11) {
  x %>% dplyr::select_if(is.factor) %>% compute_tables(x, max_distinct_values=max_distinct_values)
}

#' @rdname compute_tables
compute_tables_character <- function(x, max_distinct_values=11) {
  x %>% dplyr::select_if(is.character) %>% compute_tables(x, max_distinct_values=max_distinct_values)
}

#' @rdname compute_tables
compute_tables_logical <- function(x) {
  x %>% dplyr::select_if(is.logical) %>% compute_tables(x)
}

#' @rdname compute_tables
compute_tables_likert <- function(x) {
  x %>% dplyr::select_if(is_likert) %>% compute_tables(x)
}
