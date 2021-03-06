% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/types.R
\name{small_fixes}
\alias{small_fixes}
\alias{fix_order}
\alias{na_to_zero}
\alias{fct_lump_fnc}
\title{Small fixes to variables}
\usage{
fix_order(x)

na_to_zero(x)

fct_lump_fnc(x, max_distinct_values = 11)
}
\arguments{
\item{x}{a variable}

\item{max_distinct_values}{maximum number of factor levels to be used.}
}
\description{
Small fixes to variables
}
\details{
\code{fix_order} fixes the levels of a factor in the order of appearance

\code{na_to_zero} replaces missing values with 0

\code{fct_lump_fnc} lumps together least common levels of a factor
}
\keyword{internal}
