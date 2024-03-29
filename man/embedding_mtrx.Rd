% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embedding_mtrx.R
\name{embedding_mtrx}
\alias{embedding_mtrx}
\title{Create the embedding matrix}
\usage{
embedding_mtrx(
  mixdb_otiti_tagged,
  fasttext_pretrained,
  embedding_dim,
  max_words
)
}
\arguments{
\item{mixdb_otiti_tagged}{the \link{mixdb} containing the original data}

\item{fasttext_pretrained}{(chr) the fasttext pretrained character vector}

\item{embedding_dim}{(chr or int) the feature dimension of the output embedding}

\item{max_words}{(int) maximum number of words considered
(ranked by frequency)}
}
\value{
the embedding matrix
}
\description{
Create the embedding matrix
}
