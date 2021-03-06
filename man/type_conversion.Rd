% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/types.R
\name{type_conversion}
\alias{type_conversion}
\alias{likert2factor_fnc}
\alias{likert2revfactor_fnc}
\alias{string2factor_fnc}
\alias{logical2factor_fnc}
\alias{logical2revfactor_fnc}
\title{Type conversions}
\usage{
likert2factor_fnc(x)

likert2revfactor_fnc(x)

string2factor_fnc(x, max_distinct_values = 11)

logical2factor_fnc(x)

logical2revfactor_fnc(x)
}
\arguments{
\item{x}{a variable}

\item{max_distinct_values}{maximum number of factor levels to be used when converting a character vector. Defaults to 11.}
}
\description{
Type conversions
}
\details{
\code{likert2factor_fnc} converts a likert scale to an ordered factor

\code{likert2revfactor_fnc} converts a likert scale to a reverse-ordered factor

\code{string2factor_fnc} converts a character vector to a factor, possibly lumping smaller categories together

\code{logical2factor_fnc} converts a logical vector to an ordered factor

\code{logical2revfactor_fnc} converts a logical vector to a reverse-ordered factor
}
\keyword{internal}
