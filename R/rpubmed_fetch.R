#' Downloads abstracts and Metadata from Pubmed, storing as R objects
#' Splits large id vectors into a list of smaller chunks, so as not to hammer the entrez server! 
#' If you are making large bulk downloads, consider setting a delay so the downloading starts at off-peak USA times.
#'
#'
#' @export 
#' @import XML RCurl
#' @param ids integer Pubmed ID's to get abstracts and metadata from
#' @param chunk_size Number of articles to be pulled with each call to pubmed_fetch (optional)
#' @param delay Integer Number of hours to wait before downloading starts
#' @param \dots character Additional terms to add to the request
#' @return list containing abstratcs and metadata for each ID
#' @examples \dontrun{
#'  # Get IDs via rentrez_search:
#'  plasticity_ids <- entrez_search("pubmed", "phenotypic plasticity", retmax = 2600)$ids
#'  plasticity_records <- fetch_in_chunks(plasticity_ids)
#' } 
 


fetch_in_chunks <- function(ids, chunk_size = 500, delay = 0, ...){
    Sys.sleep(delay * 3600)         # Wait for appropriate time for the server.
    chunks <- chunker(ids, chunk_size)
    Reduce(append, lapply(chunks, function(x) pubmed_fetch(x, ...)))
}

#' Download data from Pubmed
#' 
#' 
#' 
#' @export 
#' @param ids integer Pubmed ID's to get abstracts and metadata from
#' @param file_format character Format in which to get data (eg, fasta, xml...) default = "xml"
#' @param as_r_object boolean if TRUE, parses returned xml to R objects (nested lists), else returns xml
#' @param \dots character Additional terms to add to the request
#' @return list or character string containing abstratcs and metadata for each ID (see as_r_object)
#' @examples \dontrun{
#' # Get IDs via rentrez_search:
#' plasticity_ids <- entrez_search("pubmed", "phenotypic plasticity", retmax = 2600)$ids[1:100]
#' plasticity_records <- pubmed_fetch(plasticity_ids)
#' }

pubmed_fetch <- function(ids, file_format = "xml", as_r_object = TRUE, ...){
    
    args <- c(id = paste(ids, collapse = ","), db = "pubmed", rettype = file_format, 
              email = entrez_email, tool = entrez_tool, ...)
    
    url_args <- paste(paste(names(args), args, sep="="), collapse = "&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?retmode=full"
    url_string <- paste(base_url, url_args, sep = "&")
    records <- getURL(url_string)
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    if(as_r_object){
        return(xmlToList(xmlTreeParse(records, useInternalNodes = TRUE)))
    } else return(records)
}

#' Helper function to split a vector v into list of chunks of chunk_size
chunker <- function(v, chunk_size){
    split(v, ceiling(seq_along(v)/chunk_size))
}

