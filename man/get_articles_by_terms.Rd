\name{get_articles_by_terms}
\alias{get_articles_by_terms}
\title{Returns a list of articles matching the termlist
items in the termlist can be strings or character vectors, concatenated to an "or" regex
e.g list(c("gprd", "diabetes")) returns all articles mentioning either gprd or diabetes.
different items in the list recursively filter the list
e.g. list("gprd", "diabetes") returns articles mentioning gprd and diabtes}
\usage{
  get_articles_by_terms(corpus, term_list, where,
    case_sensitive = FALSE)
}
\arguments{
  \item{corpus}{list of downloaded Pubmed records, e.g.
  from rpubmed_fetch_in_chunks}

  \item{term_list}{list of character vectors giving the
  search terms. list elements are searched for reductively
  (using &). Elements of internal charater vectors are
  combined into 'or' terms}

  \item{where}{A predicate function referring to a search
  in an area of the record. Choose from in_abstract_p,
  in_mesh_p or in_mesh_abstract_p}

  \item{case_sensitive}{boolean is the search case
  sensitive?}
}
\value{
  list containing abstracts and metadata for each ID
  matching the search criteria
}
\description{
  Returns a list of articles matching the termlist items in
  the termlist can be strings or character vectors,
  concatenated to an "or" regex e.g list(c("gprd",
  "diabetes")) returns all articles mentioning either gprd
  or diabetes. different items in the list recursively
  filter the list e.g. list("gprd", "diabetes") returns
  articles mentioning gprd and diabtes
}
\examples{
plasticity_records <- fetch_in_chunks(plasticity_ids)

# Search for articles with "plant" and "fish" in the abstract
get_articles_by_terms(plasticity_records, list("plant", "fish"), where = in_abstract_p)

# Search for records with "plant" or "fish" in the abstract or MeSH headings:
get_articles_by_terms(plasticity_records, list(c("plant", "fish")), where = in_mesh_abstract_p)
}

