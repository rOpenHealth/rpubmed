#' Write a list of (e.g.) Pubmed records (e.g.) from rpubmed_fetch_in_chunks to json file 
#' @export
#' @import RJSONIO
#' @param x object to be serialised
#' @param file filename
write_JSON_file <- function(x, file){
    cat(toJSON(x), file = file)
}

#' Redundant wrapper around fromJSON
#' @import RJSONIO
#' @param filename a filename to read from
#' @param ... arguments to be passed to fromJSON
read_article_json <- function(filename, ...){
    fromJSON(filename, ...)
}

#' Writes article title and citation data to file or stdout.  Can optionally also output abstracts and output as Markdown, with customisable line starts, e.g. for unordered lists
#' @export
#' @param articles A list of Pubmed Records e.g. as returned by fetch_in_chunks()
#' @param out_file character file to write results to. Empty string returns to stdout
#' @param abstract_p boolean Output the abstract?
#' @param linestart Character string to add at the front of each line, controlling markdown output. Default is "* " 
#' @param markdown_p boolean Output as markdown?
#' @return NULL
#' 
#'
write_record_list <- function(articles, out_file = "", abstract_p = FALSE, markdown_p = FALSE, linestart = "* "){
    appending <- FALSE
    for(article in articles){
        if(!is.null(article$PubmedArticle)) article <- article$PubmedArticle
        authors <- article$MedlineCitation$Article$AuthorList
        authors <- authors[sapply(authors, is.list)]
        if(length(authors) < 2){
            display.author <- tryCatch(with(authors[[1]], paste(LastName, ForeName, sep = ", ")),
                                       error = function(e) "Unreadable author") # bit hacky! fix!
        } else if(length(authors) == 2){
            display.author <- paste(sapply(authors[1:2], function(x) paste(x$LastName, x$ForeName, sep = ", ")), collapse = " & ")
        } else display.author <- with(authors[[1]], paste(LastName, "et al", sep = " "))
        title <- article$MedlineCitation$Article$ArticleTitle
        journal <- article$MedlineCitation$Article$Journal$Title
        volume <- article$MedlineCitation$Article$Journal$JournalIssue$Volume
        year <- article$PubmedData$History$PubMedPubDate$Year
        pages <- article$MedlineCitation$Article$Pagination[["MedlinePgn"]]
        if(is.null(pages)) pages <- "unknown page"
        abstract <- paste("Abstract:", abstract_to_text(article))
        if(markdown_p) {
            display_string <- "%s__%s. (%s)__. %s _%s_. %s: %s"
        } else {
            display_string <- "%s%s. (%s). %s %s. %s: %s"
            linestart <- ""
        }
        if(abstract_p){
            display <- sprintf(paste(display_string, "\n%s\n"),
                               linestart, display.author, year, title, journal, volume, pages, abstract)
        } else {
            display <- sprintf(display_string,
                               linestart, display.author, year, title, journal, volume, pages)
        }
        cat(display, "\n", file = out_file, append = appending)
        appending <- TRUE
    }
}

