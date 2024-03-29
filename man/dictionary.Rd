% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.R
\name{dictionary}
\alias{dictionary}
\alias{dictionary.character}
\alias{dictionary.list}
\title{Dictionary}
\usage{
dictionary(x)

\method{dictionary}{character}(x)

\method{dictionary}{list}(x)
}
\arguments{
\item{x}{an object from which create the \code{\link{dictionary}}}
}
\value{
a \code{\link{dictionary}}
}
\description{
Create a \code{\link{dictionary}} object.
}
\details{
A \code{\link{dictionary}} is an ordered and dense integer vector
representing the coding of the words in some vocabulary.
The vector is named with names corresponding to the (single) words
in the vocabulary.

It has an attribute \code{frequencies} reporting (in order from the most
frequent to the most rare) the original word-frequency of the
document(s) which created the dictionary. The \code{frequencies}
attributes is accessibly by the function \code{\link{get_frequencies}}
}
\examples{
dic <- dictionary(c("a", "b", "b"))
}
\seealso{
\code{\link{get_frequencies}}
}
