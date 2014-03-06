\name{mesh_assoc_table}
\alias{mesh_assoc_table}
\title{Builds an association matrix for all MeSH terms in an article corpus}
\usage{
mesh_assoc_table(corpus)
}
\arguments{
  \item{corpus}{a list of Pubmed records. e.g. as returned
  by fetch_in_chunks()}
}
\value{
matrix
}
\description{
Builds an association matrix for all MeSH terms in an
article corpus
}
\examples{
\dontrun{

articles <- fromJSON("Test/PCD_articles.json")
diab = get_articles_by_terms(articles, list("diabetes", c("case-control")), where= in_mesh_abstract_p)
diab_assoc <- mesh_assoc_table(diab)
# list of MeSH headings with frequencies:
d <- diag(diab_assoc)

Pick only those appearing in > 5 articles:
d_5 <- d[d > 5]
# Reduce assoc table to those with > 5 articles:
diab_5 <- diab_assoc[names(d_5), names(d_5)]

# Graph the results:
require(igraph)
g <- graph.adjacency(diab_5, mode = "upper", weighted = TRUE)
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
layout1 <- layout.fruchterman.reingold(g)
V(g)$label.cex <- V(g)$degree / max(V(g)$degree)
plot(g, layout=layout1)
}
}

