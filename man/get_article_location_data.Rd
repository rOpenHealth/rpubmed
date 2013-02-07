\name{get_article_location_data}
\alias{get_article_location_data}
\title{Extracts addresses of affiliated departments from Pubmed metadata
email addresses are cleaned out.}
\usage{
  get_article_location_data(abstracts)
}
\arguments{
  \item{abstracts}{A list of Pubmed records. e.g. from
  fetch_in_chunks()}
}
\value{
  character vector of addresses
}
\description{
  Extracts addresses of affiliated departments from Pubmed
  metadata email addresses are cleaned out.
}
\examples{
\dontrun{
# Extract affiliated addresses from article metadata:
affil_addresses <- get_article_location_data(abstracts)
}
}

