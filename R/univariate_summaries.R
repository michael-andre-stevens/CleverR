

###############################################################################

#' Downsample a dataframe
#'
#' Reduces the number of observations in a data.frame for plotting.
#'
#' @param x a data.frame
#' @param downsample downsampling factor. Defaults to "auto", in this case it keeps a maximum of 10000 observations.
#'
#' @return the downsampled data.frame
#' @keywords internal
do_downsample <- function(x, downsample="auto") {

  if (downsample=="auto") {
    downsample=10^(ceiling(log10(nrow(x)))-4) # we decimate from 100000
  }

  if (downsample>1) {
    x %>%
      dplyr::ungroup %>%
      dplyr::mutate(obs=1:dplyr::n()) %>%
      dplyr::filter(.data$obs%%downsample==0)
  } else {
    x
  }
}



###############################################################################

#' Panel functions for plotting variables
#'
#' @param x a data.frame
#' @param variable the variable that is plotted
#' @param downsample downsampling factor. Defaults to "auto", in this case it keeps a maximum of 10000 observations.
#' @details
#' \code{panel_densityplot} generates a density plot
#'
#' \code{panel_boxplot} generates a box plot
#'
#' \code{panel_qqplot} generates a quantile-quantile plot against the quantiles of the normal distribution
#'
#' \code{panel_barplot} generates a bar plot of the variable
#'
#' \code{panel_missingplot} generates a missingness plot
#'
#' @name panel_functions
#' @keywords internal
NULL
#> NULL


#' @rdname panel_functions
#' @keywords internal
panel_densityplot <- function(x, variable) {

  variable <- rlang::enquo(variable)

  x %>%
    ggplot2::ggplot(ggplot2::aes(x = !! variable)) +
    ggplot2::geom_density(col="#426890") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill='gray97', colour='gray97')
    )
}


#' @rdname panel_functions
#' @keywords internal
panel_boxplot <- function(x, variable, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    do_downsample(downsample) %>%
    ggplot2::ggplot(ggplot2::aes(x = 1, y = !! variable)) +
    ggplot2::geom_boxplot(col="#426890") +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill='gray97', colour='gray97')
    )

}


#' @rdname panel_functions
#' @keywords internal
panel_qqplot <- function(x, variable, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    do_downsample(downsample) %>%
    ggplot2::ggplot(ggplot2::aes(sample = !! variable)) +
    ggplot2::geom_qq(col="#426890") +
    ggplot2::geom_qq_line(col="#426890") +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +  # sample
    ggplot2::ylab("") +  # theoretical
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill='gray97', colour='gray97')
    )
}


#' @rdname panel_functions
#' @keywords internal
panel_barplot <- function(x, variable) {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    dplyr::mutate_if(is.numeric, likert2factor_fnc) %>%
    dplyr::mutate_if(is.logical, logical2factor_fnc) %>%
    dplyr::group_by(!! variable) %>%
    dplyr::summarize(n=dplyr::n()) %>%
    dplyr::arrange(.data$n) %>%
    dplyr::mutate_if(is_unordered, fix_order) %>%
    dplyr::arrange(!! variable) %>%
    ggplot2::ggplot(ggplot2::aes(x = !! variable, y = .data$n)) +
    ggplot2::geom_bar(stat="identity", fill="#426890", width=0.6) +
    ggplot2::scale_y_continuous(limits=c(0,NA)) +
    ggplot2::xlab("") +
    ggplot2:: ylab("") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill='gray97', colour='gray97')
    )
}


#' @rdname panel_functions
#' @keywords internal
panel_missingplot <- function(x, variable, downsample="auto") {

  variable <- rlang::enquo(variable)

  my_missing <- function(x) {
    is.na(x) %>%
      ordered(levels=c(FALSE, TRUE), labels=c("Observed", "Missing"))
  }

  x %>%
    dplyr::select(!! variable) %>%
    do_downsample(downsample) %>%
    dplyr::mutate(missing = my_missing(!! variable)) %>%
    dplyr::mutate(x=1:dplyr::n(), y=".") %>%
    ggplot2::ggplot(ggplot2::aes(x=.data$x, y=.data$y, fill=.data$missing)) +
    ggplot2::geom_raster() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_manual(
      name = "",
      labels = c("Observed","Missing"),
      values = c("#426890", scales::alpha("gray92", 0)),
      drop = FALSE) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill='gray97', colour='gray97'),
      legend.background= ggplot2::element_rect(fill='gray97', colour='gray97')
    )
}


###############################################################################

#' Summary plots for variables
#'
#' @param x a data.frame
#' @param variable the variable that is plotted
#' @param downsample downsampling factor. Defaults to "auto", in this case it keeps a maximum of 10000 observations
#' @param max_distinct_values the maximum number of different levels or values in a factor or string variable
#' @details
#' \code{plot_continuous} generates plots for continuous variables: a density plot, a box plot, a qq plot and a missingness plot
#'
#' \code{plot_likert} generates plots for likert variables. Similar to a continuous plot, but the density plot is replaced by a bar plot.
#'
#' \code{plot_posixt} generates plots for times. The same as the continuous plot but without the summary statistics
#'
#' \code{plot_factor} generates plots for factor variables: a bar plot and a missingness plot
#'
#' \code{plot_string} generates plots for string variables. The string is converted to a factor with \code{max_distinct_values} levels, then the factor plots are applied.
#'
#' \code{plot_logical} generates plots for logical variables. The string is converted to a factor with two levels, then the factor plots are applied.
#'
#' @name plot_univariate
NULL
#> NULL


#' @rdname plot_univariate
#' @export
plot_continuous <- function(x, variable, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    column_func(labels_func, "label") %>%
    dplyr::pull(.data$label) %>%
    cat()

  writeLines("")

  plot1 <- panel_densityplot(x, !! variable)
  plot2 <- panel_boxplot(x, !! variable, downsample=downsample)
  plot3 <- panel_qqplot(x, !! variable, downsample=downsample)
  plot4 <- panel_missingplot(x, !! variable, downsample=downsample)

  gridExtra::grid.arrange(plot1, plot2, plot3, plot4, layout_matrix=rbind(c(1,2,3),c(4,4,4)), heights=c(4,1))

  x %>% dplyr::select(!! variable)  %>% compute_descriptives() %>% dplyr::select(-variable, -class) %>% print()
}



#' @rdname plot_univariate
#' @export
plot_likert <- function(x, variable, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    column_func(labels_func, "label") %>%
    dplyr::pull(.data$label) %>%
    cat()

  writeLines("")

  plot1 <- panel_barplot(x, !! variable)
  plot2 <- panel_boxplot(x, !! variable, downsample=downsample)
  plot3 <- panel_qqplot(x, !! variable, downsample=downsample)
  plot4 <- panel_missingplot(x, !! variable, downsample=downsample)

  gridExtra::grid.arrange(plot1, plot2, plot3, plot4, layout_matrix=rbind(c(1,2,3),c(4,4,4)), heights=c(4,1))

  x %>% dplyr::select(!! variable) %>% compute_descriptives() %>% dplyr::select(-variable, -class) %>% print()
  writeLines("")
  x %>% dplyr::select(!! variable) %>% compute_tables() %>% dplyr::select(-variable) %>% print()
}


#' @rdname plot_univariate
#' @export
plot_posixt <- function(x, variable, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    column_func(labels_func, "label") %>%
    dplyr::pull(.data$label) %>%
    cat()

  writeLines("")

  plot1 <- panel_densityplot(x, !! variable)
  plot2 <- panel_boxplot(x, !! variable, downsample=downsample)
  plot3 <- panel_qqplot(x, !! variable, downsample=downsample)
  plot4 <- panel_missingplot(x, !! variable, downsample=downsample)

  gridExtra::grid.arrange(plot1, plot2, plot3, plot4, layout_matrix=rbind(c(1,2,3),c(4,4,4)), heights=c(4,1))

  #x %>% dplyr::select(!! variable)  %>% compute_descriptives() %>% dplyr::select(-variable, -class) %>% print()
}


#' @rdname plot_univariate
#' @export
plot_factor <- function(x, variable, max_distinct_values=11, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    column_func(labels_func, "label") %>%
    dplyr::pull(.data$label) %>%
    cat()

  writeLines("")

  x <- x %>%
    dplyr::mutate(lumped_variable = fct_lump_fnc(!! variable, max_distinct_values))

  plot1 <- panel_barplot(x, .data$lumped_variable) + ggplot2::coord_flip()
  plot2 <- panel_missingplot(x, .data$lumped_variable, downsample=downsample)

  ncat <- length(levels(x$lumped_variable))

  if(any(is.na(x$lumped_variable))) {
    ncat <- ncat + 1
  }

  gridExtra::grid.arrange(plot1, plot2, nrow=2, heights=c(ncat+2,2))

  x %>%
    dplyr::select(.data$lumped_variable) %>%
    compute_tables() %>%
    dplyr::select(-variable) %>%
    print()
}


#' @rdname plot_univariate
#' @export
plot_string <- function(x, variable, max_distinct_values=11, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    column_func(labels_func, "label") %>%
    dplyr::pull(.data$label) %>%
    cat()

  writeLines("")

  x <- x %>%
    dplyr::mutate(factorized_variable = string2factor_fnc(!! variable, max_distinct_values))

  plot1 <- panel_barplot(x, .data$factorized_variable) + ggplot2::coord_flip()
  plot2 <- panel_missingplot(x, .data$factorized_variable, downsample=downsample)

  ncat <- length(levels(x$factorized_variable))

  if(any(is.na(x$factorized_variable))) {
    ncat <- ncat + 1
  }

  gridExtra::grid.arrange(plot1, plot2, nrow=2, heights=c(ncat+2,2))

  x %>%
    dplyr::select(.data$factorized_variable) %>%
    compute_tables() %>%
    dplyr::select(-variable) %>%
    print()
}


#' @rdname plot_univariate
#' @export
plot_logical <- function(x, variable, downsample="auto") {

  variable <- rlang::enquo(variable)

  x %>%
    dplyr::select(!! variable) %>%
    column_func(labels_func, "label") %>%
    dplyr::pull(.data$label) %>%
    cat()

  writeLines("")

  x <- x %>%
    dplyr::mutate(factorized_variable = ordered(!! variable, levels=c(FALSE, TRUE), labels=c("FALSE", "TRUE")))

  plot1 <- panel_barplot(x, .data$factorized_variable) + ggplot2::coord_flip()
  plot2 <- panel_missingplot(x, .data$factorized_variable, downsample=downsample)

  ncat <- length(levels(x$factorized_variable))

  if(any(is.na(x$factorized_variable))) {
    ncat <- ncat + 1
  }

  gridExtra::grid.arrange(plot1, plot2, nrow=2, heights=c(ncat+2,2))

  x %>%
    dplyr::select(.data$factorized_variable) %>%
    compute_tables() %>%
    dplyr::select(-variable) %>%
    print()
}



###############################################################################

#' Generate rmarkdown to plot all variables in a dataframe
#'
#' @param x a data.frame
#'
#' @return prints out markdown on the console.
#' @export
markdown_univariate <- function(x) {


  # height:
  # continuous:     3 (2.4+0.6)
  # string:    (10 + 2)/4 + 0.6
  # categorical:  (ncat + 2)/4 + 0.6

  # get the name of the data frame
  dataname <- deparse(substitute(x))

  for (variable in names(x)) {

    xx <- x %>% dplyr::pull(variable)


    if(is.integer(xx) ) {
      ncat=length(unique(xx))
      if (ncat<20) {
        plot_type="plot_likert"
      } else {
        plot_type="plot_continuous"
      }
      plot_height=(2.4+0.6)
    } else if(is.numeric(xx)) {
      plot_type="plot_continuous"
      plot_height=(2.4+0.6)
    } else if (lubridate::is.POSIXt(xx)) {
      plot_type="plot_posixt"
      plot_height=(2.4+0.6)
    } else if (is.factor(xx)) {
      plot_type="plot_factor"
      ncat=min(length(unique(xx)), 11)
      plot_height=paste0("(", ncat, " + 2)/4 + 0.6")
    } else if (is.logical(xx)) {
      plot_type="plot_logical"
      ncat=length(unique(xx))
      plot_height=paste0("(", ncat, " + 2)/4 + 0.6")
    } else if (is.character(xx)) {
      plot_type="plot_string"
      ncat=min(length(unique(xx)), 11)
      plot_height=paste0("(", ncat, " + 2)/4 + 0.6")
    } else {
      plot_type="plot_string"
      plot_height=0.6
    }

    cat(paste("#", variable, "\n"))
    cat(paste("\n"))
    cat(paste("```{r fig.height=", plot_height, "}\n", sep=""))
    cat(paste(plot_type, "(", dataname, ", `", variable, "`)", "\n", sep=""))
    cat(paste("```\n"))
    cat(paste("\n\n"))
  }
}



###############################################################################

#' Grouped missingness plot
#'
#' @param x a data.frame
#' @param downsample downsampling factor. Defaults to "auto", in this case it keeps a maximum of 10000 observations.
#'
#' @export
missing_group_plot <- function(x, downsample="auto") {

  x %>%
    do_downsample(downsample) %>%
    is.na() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(obs=1:dplyr::n()) %>%
    tidyr::gather('variable', 'missing', -'obs') %>%
    dplyr::mutate(variable=forcats::as_factor(.data$variable)) %>%
    dplyr::mutate(variable=factor(.data$variable, levels=rev(levels(.data$variable)))) %>%
    dplyr::mutate(missing=ordered(.data$missing, levels=c(FALSE, TRUE), labels=c("Observed", "Missing"))) %>%
    ggplot2::ggplot(ggplot2::aes(x=.data$obs, y=.data$variable, fill=.data$missing)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(
      name = "", values = c("#426890", scales::alpha("gray92", 0)), drop=FALSE) +
    ggplot2::theme(
      axis.title.x=ggplot2::element_blank(),
      axis.text.x=ggplot2::element_blank(),
      axis.ticks.x=ggplot2::element_blank(),
      axis.title.y=ggplot2::element_blank())
}

#' Grouped Likert plot
#'
#' @param x a data.frame
#'
#' @export
likert_group_plot <- function(x) {

  x %>%
    dplyr::mutate_all(as.factor) %>%
    data.frame() %>%
    likert::likert() %>%
    likert::likert.bar.plot(low.color = "#426890", high.color = "#8F4243") + #group.order=variables) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme_grey() +
    ggplot2::theme(plot.background =  ggplot2::element_rect(fill='gray97', colour='gray97')) +
    ggplot2::theme(legend.background=  ggplot2::element_rect(fill='gray97', colour='gray97')) +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::guides(fill= ggplot2::guide_legend(title="", nrow=1))
}






