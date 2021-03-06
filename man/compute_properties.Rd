% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_summaries.R
\name{compute_properties}
\alias{compute_properties}
\title{Compute a set of properties for all variables (columns) in a dataframe}
\usage{
compute_properties(x)
}
\arguments{
\item{x}{a tibble}
}
\value{
a tibble with variables in the rows and properties in the columns
}
\description{
Compute a set of properties for all variables (columns) in a dataframe
}
\examples{
datasets::mtcars \%>\% compute_properties()
}
