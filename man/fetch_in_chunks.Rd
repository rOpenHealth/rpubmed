\name{fetch_in_chunks}
\alias{fetch_in_chunks}
\title{Downloads abstracts and Metadata from Pubmed, storing as R objects
Splits large id vectors into a list of smaller chunks, so as not to hammer the entrez server!
If you are making large bulk downloads, consider setting a delay so the downloading starts at off-peak USA times.}
\usage{
  fetch_in_chunks(ids, chunk_size = 500, delay = 0, ...)
}
\arguments{
  \item{ids}{integer Pubmed ID's to get abstracts and
  metadata from}

  \item{chunk_size}{Number of articles to be pulled with
  each call to pubmed_fetch (optional)}

  \item{delay}{Integer Number of hours to wait before
  downloading starts}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  list containing abstratcs and metadata for each ID
}
\description{
  Downloads abstracts and Metadata from Pubmed, storing as
  R objects Splits large id vectors into a list of smaller
  chunks, so as not to hammer the entrez server! If you are
  making large bulk downloads, consider setting a delay so
  the downloading starts at off-peak USA times.
}
\examples{
# Get IDs via rentrez_search:
plasticity_ids <- entrez_search("pubmed", "phenotypic plasticity", retmax = 2600)$ids
plasticity_records <- fetch_in_chunks(plasticity_ids)
}

