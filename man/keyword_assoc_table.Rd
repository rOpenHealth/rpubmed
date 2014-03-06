\name{keyword_assoc_table}
\alias{keyword_assoc_table}
\title{Builds an association table for a character vector of search terms in a corpus. This can then e.g. be fed into igraph to generate an adjacency graph of terms
Different column names can be set for the association matrix if e.g. complex regex terms are used for the keyword_list}
\usage{
keyword_assoc_table(corpus, keyword_list, keyword_names = NULL, ...)
}
\arguments{
  \item{corpus}{a list of Pubmed records. e.g. as returned
  by fetch_in_chunks()}

  \item{keyword_list}{Character vector of keywords}

  \item{keyword_names}{NULL or Character vector of equal
  length to keyword_list}

  \item{...}{arguments to be passed to
  get_articles_by_terms. e.g.  where = in_mesh_abstract_p}
}
\value{
matrix
}
\description{
Builds an association table for a character vector of
search terms in a corpus. This can then e.g. be fed into
igraph to generate an adjacency graph of terms Different
column names can be set for the association matrix if e.g.
complex regex terms are used for the keyword_list
}
\examples{
\dontrun{
     articles <- fromJSON("Test/PCD_articles.json")
     keywords <- c("effectiveness", "treatment outcome", "comorbidity", "risk factor", "incidence")
     k_names <- c("EFF", "TREATOUT", "COMORB", "RISK", "INCID")
     kat <- keyword_assoc_table(corpus = articles, keyword_list = keywords, keyword_names = k_names, where = in_mesh_abstract_p)
 }
}

