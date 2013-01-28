\name{pubmed_fetch}
\alias{pubmed_fetch}
\title{Download data from Pubmed}
\usage{
  pubmed_fetch(ids, file_format = "xml",
    as_r_object = TRUE, ...)
}
\arguments{
  \item{ids}{integer Pubmed ID's to get abstracts and
  metadata from}

  \item{file_format}{character Format in which to get data
  (eg, fasta, xml...) default = "xml"}

  \item{as_r_object}{boolean if TRUE, parses returned xml
  to R objects (nested lists), else returns xml}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  list or character string containing abstratcs and
  metadata for each ID (see as_r_object)
}
\description{
  Download data from Pubmed
}
\examples{
# Get IDs via rentrez_search:
plasticity_ids <- entrez_search("pubmed", "phenotypic plasticity", retmax = 2600)$ids[1:100]
plasticity_records <- pubmed_fetch(plasticity_ids)
}

