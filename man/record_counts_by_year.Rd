\name{record_counts_by_year}
\alias{record_counts_by_year}
\title{Gives a breakdown of records per year in a corpus of Pubmed Records}
\usage{
  record_counts_by_year(corpus, year_min = FALSE,
    year_max = FALSE)
}
\arguments{
  \item{corpus}{a list of Pubmed records e.g. as given by
  fetch_in_chunks()}

  \item{year_min}{integer representing the earliest year to
  be included in the counts}

  \item{year_max}{integer representing the latest year to
  be included in the counts. Frequencies are calculated
  after the dataset is truncated.}
}
\value{
  dataframe with year, records and freq columns
}
\description{
  Gives a breakdown of records per year in a corpus of
  Pubmed Records
}

