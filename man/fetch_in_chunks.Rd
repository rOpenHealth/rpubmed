\name{fetch_in_chunks}
\alias{fetch_in_chunks}
\title{Downloads abstracts and Metadata from Pubmed, storing as R objects
Splits large id vectors into a list of smaller chunks, so as not to hammer the entrez server!}
\usage{
  fetch_in_chunks(ids, chunk_size = 500, ...)
}
\arguments{
  \item{ids}{integer Pubmed ID's to get abstracts and
  metadata from}

  \item{chunk_size}{Number of articles to be pulled with
  each call to pubmed_fetch (optional)}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  list containing abstratcs and metadata for each ID
}
\description{
  Downloads abstracts and Metadata from Pubmed, storing as
  R objects Splits large id vectors into a list of smaller
  chunks, so as not to hammer the entrez server!
}
\examples{
# Get IDs via rentrez_search:
plasticity_ids <- entrez_search("pubmed", "phenotypic plasticity", retmax = 2600)$ids
plasticity_records <- fetch_in_chunks(plasticity_ids)
}

