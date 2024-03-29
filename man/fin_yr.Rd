% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{fin_yr}
\alias{fin_yr}
\title{Calculate financial year and quarter from a date}
\usage{
fin_yr(x, fs = 7)
}
\arguments{
\item{x}{a year object}

\item{fs}{start month in numerical representation. Defaults to 7 for a standard
Australian finacial year.}
}
\value{
a character string in the format YYYY_Q, where YYYY is the finacial
year and Q is the numerical quarter of that finacial year.
}
\description{
\code{fin_yr} takes a date object and a statring month and calculates a financial
year and quarter.
}
\examples{
\dontrun{
f_q <- fin_yr(x = 2023-02-23, fs = 7)
}

}
\author{
Bart Huntley, \email{bart.huntley@dbca.wa.gov.au}
}
\keyword{internal}
