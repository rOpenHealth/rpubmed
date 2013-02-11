\name{keyword_assoc_table}
\alias{keyword_assoc_table}
\title{Builds an association table for a character vector of search terms in a corpus. This can then e.g. be fed into igraph to generate an adjacency graph of terms}
\usage{
  keyword_assoc_table(corpus, keyword_list, ...)
}
\arguments{
  \item{corpus}{a list of Pubmed records. e.g. as returned
  by fetch_in_chunks()}

  \item{keyword_list}{Character vector of keywords}

  \item{...}{arguments to be passed to
  get_articles_by_terms. e.g.  where = in_mesh_abstract_p}
}
\value{
  matrix
}
\description{
  Builds an association table for a character vector of
  search terms in a corpus. This can then e.g. be fed into
  igraph to generate an adjacency graph of terms
}
\examples{
\dontrun{
     articles <- fromJSON("Test/PCD_articles.json")
     keywords <- c("effectiveness", "treatment outcome", "comorbidity", "risk factor", "incidence")
     kat <- keyword_assoc_table(corpus = articles, eyword_list = keywords, where = in_meah_abstract_p)
 }
}

