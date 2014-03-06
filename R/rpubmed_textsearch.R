#' Returns a list of articles matching the termlist
#' items in the termlist can be strings or character vectors, concatenated to an "or" regex
#' e.g list(c("gprd", "diabetes")) returns all articles mentioning either gprd or diabetes.
#' different items in the list recursively filter the list
#' e.g. list("gprd", "diabetes") returns articles mentioning gprd and diabtes
#'
#' @export 
#' @param corpus list of downloaded Pubmed records, e.g. from rpubmed_fetch_in_chunks
#' @param term_list list of character vectors giving the search terms. list elements are searched for reductively (using &). Elements of internal charater vectors are combined into 'or' terms
#' @param where A predicate function referring to a search in an area of the record. Choose from in_abstract_p, in_mesh_p or in_mesh_abstract_p
#' @param case_sensitive boolean is the search case sensitive?
#' @param ... arguments to be passed down to grep, e.g. invert = TRUE, perl = TRUE
#' @return list containing abstracts and metadata for each ID matching the search criteria
#' @examples \dontrun{
#' plasticity_records <- fetch_in_chunks(plasticity_ids)
#' 
#' # Search for articles with "plant" and "fish" in the abstract
#' get_articles_by_terms(plasticity_records, list("plant", "fish"), where = in_abstract_p)
#' 
#' # Search for records with "plant" or "fish" in the abstract or MeSH headings:
#' get_articles_by_terms(plasticity_records, list(c("plant", "fish")), where = in_mesh_abstract_p)
#' }
get_articles_by_terms <- function(corpus, term_list, where, case_sensitive = FALSE, ...){
    corpus <- lapply(corpus, 
                     function(article){
                         if(where(article, term_list[[1]], case_sensitive, ...)) article
                     })
    corpus <- corpus[!sapply(corpus, is.null)]
    if(length(term_list) <= 1){
        return(corpus)
    } else {
        return(get_articles_by_terms(corpus, term_list[2:length(term_list)], where = where))
    }
}

#' Gives a breakdown of records per year in a corpus of Pubmed Records
#' 
#' @export
#' @param corpus a list of Pubmed records e.g. as given by fetch_in_chunks()
#' @param year_min integer representing the earliest year to be included in the counts
#' @param year_max integer representing the latest year to be included in the counts. Frequencies are calculated after the dataset is truncated.
#' @return dataframe with year, records and freq columns
#' 
#' 
record_counts_by_year <- function(corpus, year_min = FALSE, year_max = FALSE){
    if(!length(corpus)) return(data.frame())
    years_table <- table(sapply(corpus, 
           function(x) as.numeric(x$PubmedData$History$PubMedPubDate$Year), 
           simplify = TRUE))
    years_df <- data.frame(years_table)
    names(years_df) <- c("year", "records")
    years_df$year <- as.integer(as.character(years_df$year))
    if(is.numeric(year_min)) years_df <- years_df[years_df$year >= year_min,]
    if(is.numeric(year_max)) years_df <- years_df[years_df$year <= year_max,]
    years_df$freq <- with(years_df, years_df$records / sum(years_df$records))
    years_df
}


# Helper functions:

#' concatenates abstract list to a single string
#' @param article R representation of a pubmed record
abstract_to_text <- function(article){
    paste(unlist(article$MedlineCitation$Article$Abstract), collapse = " ")
}

#' concatenates a list of MeSH headings to a single string
#' @param article R representation of a pubmed record
mesh_to_text <- function(article){
    paste(unlist(get_mesh_headings(article)), collapse = " ")
}

#' concatenates a list of MeSH headings to a single string
#' @param article R representation of a pubmed record
title_to_text <- function(article){
    paste(unlist(article$MedlineCitation$Article$ArticleTitle), collapse = " ")
}


#' predicate function for presence of a term in an article text
#' @param term string
#' @param text article text
#' @param case_sensitive logical
#' @param ... arguments to be passed to grep
term_in_text_p <- function(term, text, case_sensitive, ...){
    ifelse(length(grep(pattern = term, x = text, ignore.case = !case_sensitive, ...)), TRUE, FALSE)
}

#' predicate function for searching abstracts 
#' @param article R representation of a pubmed record
#' @param terms vector of terms
#' @param case_sensitive logical
#' @param ... arguments to be passed to grep
in_abstract_p <- function(article, terms, case_sensitive = FALSE, ...){
    # are terms found in the abstract body?
    pattern <- paste(terms, collapse = "|")
    term_in_text_p(term = pattern,
                    text = abstract_to_text(article), case_sensitive, ...)
}

#' predicate function for searching MeSH headings
#' @param article R representation of a pubmed record
#' @param terms vector of terms
#' @param case_sensitive logical
#' @param ... arguments to be passed to grep
in_mesh_headings_p <- function(article, terms, case_sensitive = FALSE, ...){
    # Are terms found in the mesh headings?
    pattern <- paste(terms, collapse = "|")
    mesh <- mesh_to_text(article)
    term_in_text_p(term = pattern,
                    text = mesh, case_sensitive, ...)
}

#' predicate function for searching abstracts and MeSH headings
#' @param article R representation of a pubmed record
#' @param terms vector of terms
#' @param case_sensitive logical
#' @param ... arguments to be passed to grep
in_mesh_abstract_p <- function(article, terms, case_sensitive = FALSE, ...){
    # Are terms found in the mesh headings?
    pattern <- paste(terms, collapse = "|")
    mesh_article <- paste(mesh_to_text(article), abstract_to_text(article))
    term_in_text_p(term = pattern,
                    text = mesh_article, case_sensitive, ...)
}

#' predicate function for searching in title and abstract
#' @param article R representation of a pubmed record
#' @param terms vector of terms
#' @param case_sensitive logical
#' @param ... arguments to be passed to grep
in_record_text_p <- function(article, terms, case_sensitive = FALSE, ...){
    # Are terms found in the mesh headings?
    pattern <- paste(terms, collapse = "|")
    mesh_article <- paste(title_to_text(article), abstract_to_text(article))
    term_in_text_p(term = pattern,
                   text = mesh_article, case_sensitive, ...)
}


