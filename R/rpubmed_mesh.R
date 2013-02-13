#' Builds an association matrix for all MeSH terms in an article corpus
#' @export
#' @param corpus a list of Pubmed records. e.g. as returned by fetch_in_chunks()
#' @return matrix
#' @examples \dontrun{
#' 
#' articles <- fromJSON("Test/PCD_articles.json")
#' diab = get_articles_by_terms(articles, list("diabetes", c("case-control")), where= in_mesh_abstract_p)
#' diab_assoc <- mesh_assoc_table(diab)
#' # list of MeSH headings with frequencies:
#' d <- diag(diab_assoc)
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
mesh_assoc_table <- function(corpus){
    m_table <- mesh_table(corpus)
    assoc_table <- matrix(0, nrow = length(levels(m_table$X1)), ncol = length(levels(m_table$X1)), 
                          dimnames = list(levels(m_table$X1)[ordered = TRUE], levels(m_table$X1)[ordered = TRUE]))
    m_table$X1 <- as.character(m_table$X1)
    m_table$X2 <- as.character(m_table$X2)
    for(i in 1:nrow(m_table)){
        assoc_table[[m_table[i, "X1"], m_table[i, "X2"]]] <- assoc_table[[m_table[i, "X1"], m_table[i, "X2"]]] + 1
    }
    assoc_table
}

#' helper function for mesh_assoc_table
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

#' Builds an association table for a character vector of search terms in a corpus. This can then e.g. be fed into igraph to generate an adjacency graph of terms 
#' Different column names can be set for the association matrix if e.g. complex regex terms are used for the keyword_list
#' @export
#' @param corpus a list of Pubmed records. e.g. as returned by fetch_in_chunks()
#' @param keyword_list Character vector of keywords
#' @param keyword_names NULL or Character vector of equal length to keyword_list
#' @param ... arguments to be passed to get_articles_by_terms. e.g.  where = in_mesh_abstract_p
#' 
#' @return  matrix
#'  @examples \dontrun{
#'      articles <- fromJSON("Test/PCD_articles.json")
#'      keywords <- c("effectiveness", "treatment outcome", "comorbidity", "risk factor", "incidence")
#'      k_names <- c("EFF", "TREATOUT", "COMORB", "RISK", "INCID")
#'      kat <- keyword_assoc_table(corpus = articles, keyword_list = keywords, keyword_names = k_names, where = in_mesh_abstract_p)
#'  }
keyword_assoc_table <- function(corpus, keyword_list, keyword_names = NULL, ...){
    kt <- data.frame(rbind(t(combn(keyword_list, 2)),
                           matrix(rep(keyword_list, each = 2), ncol= 2, byrow = TRUE)), stringsAsFactors = FALSE)
    kt$count <- sapply(1:nrow(kt), 
                       function(i){
                           if(kt[i,1] == kt[i,2]){
                               length(get_articles_by_terms(corpus = corpus, 
                                                            term_list= list(kt[i,1]), ...))
                           } else{
                               length(get_articles_by_terms(corpus = corpus, 
                                                            term_list= lapply(kt[i,], function(x) x), ...))
                           }
                       })
    
    if(!is.null(keyword_names)){
        if(length(keyword_names) != length(keyword_list)) stop("keyword_list and keyword_names are different lengths")
        kt$Xfact <- factor(sapply(kt$X1, function(x) keyword_names[which(keyword_list %in% x)]))
        kt$Xfact2 <- factor(sapply(kt$X2, function(x) keyword_names[which(keyword_list %in% x)]))
    } else {
        kt$Xfact <- factor(kt$X1)    
        kt$Xfact2 <- factor(kt$X2)
    }
    assoc_table <- matrix(0, nrow = length(levels(kt$Xfact)), ncol = length(levels(kt$Xfact)), 
                          dimnames = list(levels(kt$Xfact)[ordered = TRUE], levels(kt$Xfact)[ordered = TRUE]))
    kt$Xfact <- as.character(kt$Xfact)
    kt$Xfact2 <- as.character(kt$Xfact2)
    
    for(i in 1:nrow(kt)){
        assoc_table[[kt[i, "Xfact"], kt[i, "Xfact2"]]] <- kt$count[i]
    }
    assoc_table
}


#' Returns a list of MeSH headings for an article
#' @export
#' @param article List representing a single Pubmed Record e.g. an element from a list returned from fetch_in_chunks()
#' @return list of MeSH Headings
#' 
#' 
get_mesh_headings <- function(article){
    lapply(article$MedlineCitation$MeshHeadingList, 
           function(x) {
               if(is.null(x$DescriptorName$text)) x$DescriptorName
               else x$DescriptorName$text
           })
}

#' Returns a data frame of all MeSH headings in a corpus, with frequencies for each
#' @export
#' @param article List representing a single Pubmed Record e.g. an element from a list returned from fetch_in_chunks()
#' @return list of MeSHHeadings
#' 
#' 
mesh_heading_frequency <- function(corpus){
    ft <- data.frame(table(as.character(unlist(lapply(corpus, 
                                                      function(record) as.character(unlist(get_mesh_headings(record))))))))
    names(ft) <- c("MeSH_heading", "frequency")
    ft[order(-ft$frequency),]
}

