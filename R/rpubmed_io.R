# Document this page properly!
#require(RJSONIO)

# Write a list of (e.g.) Pubmed records (e.g.) from rpubmed_fetch_in_chunks to json file 
write_JSON_file <- function(x, file){
    cat(toJSON(x), file = file)
}

# Writes article title, citation info and abstracts to file or sdout
write_abstracts <- function(articles, out_file = ""){
    appending <- FALSE
    for(article in articles){
        authors <- article$MedlineCitation$Article$AuthorList
        cat(article$MedlineCitation$Article$ArticleTitle,
            paste(lapply(authors[sapply(authors, is.list)], 
                         function(x) if(is.list(x)) paste(x$ForeName, x$LastName)), collapse = ", "),
            paste(article$MedlineCitation$Article$Journal$Title, " vol. ",
                  article$MedlineCitation$Article$Journal$JournalIssue$Volume, "(",
                  article$MedlineCitation$Article$Journal$JournalIssue$PubDate[["Year"]], ")", sep=""), 
            paste("Abstract:", abstract_to_text(article)), "\n", 
            sep = "\n", file = out_file, append = appending)
        appending <- TRUE
    }
}

# Writes article title and citation data to file or stdout
write_record_list <- function(articles, out_file = ""){
    # Writes article title and citation data to file or stdout
    appending <- FALSE
    for(article in articles){
        authors <- article$MedlineCitation$Article$AuthorList
        authors <- authors[sapply(authors, is.list)]
        if(length(authors) == 1){
            display.author <- tryCatch(with(authors[[1]], paste(LastName, ForeName, sep = ", ")),
                                       error = function(e) "Unreadable author") # bit hacky! fix!
        } else if(length(authors) == 2){
            display.author <- paste(sapply(authors[1:2], function(x) paste(x$LastName, x$ForeName, sep = ", ")), collapse = " & ")
        } else display.author <- with(authors[[1]], paste(LastName, "et al", sep = " "))
        title <- article$MedlineCitation$Article$ArticleTitle
        journal <- article$MedlineCitation$Article$Journal$Title
        volume <- article$MedlineCitation$Article$Journal$JournalIssue$Volume
        year <- article$MedlineCitation$Article$Journal$JournalIssue$PubDate[["Year"]]
        pages <- article$MedlineCitation$Article$Pagination[["MedlinePgn"]]
        display <- sprintf("%s. (%s). %s %s. %s: %s",
                                       display.author, year, title, journal, volume, pages)
        cat(display, "\n", file = out_file, append = appending)
        appending <- TRUE
    }
}





