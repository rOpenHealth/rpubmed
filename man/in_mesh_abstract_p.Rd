\name{in_mesh_abstract_p}
\alias{in_mesh_abstract_p}
\title{predicate function for searching abstracts and MeSH headings}
\usage{
in_mesh_abstract_p(article, terms, case_sensitive = FALSE, ...)
}
\arguments{
  \item{article}{R representation of a pubmed record}

  \item{terms}{vector of terms}

  \item{case_sensitive}{logical}

  \item{...}{arguments to be passed to grep}
}
\description{
predicate function for searching abstracts and MeSH
headings
}

