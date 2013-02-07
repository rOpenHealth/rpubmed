
#'
#' @examples \dontrun{
#' 
#' articles <- fromJSON("Test/PCD_articles.json")
#' a = mesh_table(articles)
#' diab = get_articles_by_terms(articles, list("diabetes", c("case-control")), where= in_mesh_abstract_p)
#' diab_tab <- mesh_table(diab)
#' diab_assoc <- mesh_assoc_table(diab_tab)
#' # list of MeSH headings with frequencies:
#' d <- diag(ccsa)
#' 
#' Pick only those appearing in > 5 articles:
#' d_5 <- d[d > 5]
#' # Reduce assoc table to those with > 5 articles:
#' diab_5 <- diab_assoc[names(d_5), names(d_5)]
#' 
#' # Graph the results: 
#' require(igraph)
#' g <- graph.adjacency(diab_5, mode = "upper", weighted = TRUE)
#' g <- simplify(g)
#' V(g)$label <- V(g)$name
#' V(g)$degree <- degree(g)
#' layout1 <- layout.fruchterman.reingold(g)
#' V(g)$label.cex <- V(g)$degree / max(V(g)$degree)
#' plot(g, layout=layout1)
#' }
#' 
mesh_assoc_table <- function(m_table){
    assoc_table <- matrix(0, nrow = length(levels(m_table$X1)), ncol = length(levels(m_table$X1)), 
                          dimnames = list(levels(m_table$X1)[ordered = TRUE], levels(m_table$X1)[ordered = TRUE]))
    m_table$X1 <- as.character(m_table$X1)
    m_table$X2 <- as.character(m_table$X2)
    for(i in 1:nrow(m_table)){
        assoc_table[[m_table[i, "X1"], m_table[i, "X2"]]] <- assoc_table[[m_table[i, "X1"], m_table[i, "X2"]]] + 1
    }
    assoc_table
}

mesh_table <- function(articles){
    m_table <- data.frame(do.call(rbind, 
                                  lapply(1:length(articles), 
                                         function(article_num){
                                             mesh <- unlist(get_mesh_headings(articles[[article_num]]))
                                             if(is.vector(mesh)){
                                                 if(length(mesh) > 1){
                                                     mt <- rbind(t(combn(mesh, 2)),                                     # combinations of different headings
                                                                 matrix(rep(mesh, each = 2), ncol = 2, byrow = TRUE))   # individual headings        
                                                 } else mt <- matrix(rep(mesh, each = 2), ncol = 2, byrow = TRUE)
                                                 cbind(mt, article_num)    
                                             }
                                         })))
    names(m_table) <- c("X1", "X2", "study_id")
    m_table
}


#' Returns a list of MeSH headings for an article
#' @export
#' @param article List representing a single Pubmed Record e.g. an element from a list returned from fetch_in_chunks()
#' @return list of MeSHHeadings
#' 
#' 
get_mesh_headings <- function(article){
    lapply(article$MedlineCitation$MeshHeadingList, 
           function(x) {
               if(is.null(x$DescriptorName$text)) x$DescriptorName
               else x$DescriptorName$text
           })
}

mesh_heading_frequency <- function(corpus){
    ft <- data.frame(table(as.character(unlist(lapply(corpus, 
                                                      function(record) as.character(unlist(get_mesh_headings(record))))))))
    names(ft) <- c("MeSH_heading", "frequency")
    ft[order(-ft$frequency),]
}

