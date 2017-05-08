#' Downloads abstracts and Metadata from Pubmed, storing as R objects
#'
#' Splits large id vectors into a list of smaller chunks, so as not to hammer the entrez server!
#'
#' If you are making large bulk downloads, consider setting a delay so the downloading starts at off-peak USA times.
#'
#' @export
#' @import XML RCurl rentrez
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
    Reduce(append, lapply(chunks, function(x) {
     recs <- pubmed_fetch(x, ...)
     Sys.sleep(3)
     recs
    }))
}

#' Download data from Pubmed
#'
#'
#' @export
#' @param ids integer Pubmed ID's to get abstracts and metadata from
#' @param \dots character Additional terms to add to entrez_fetch
#' @return list or character string containing abstratcs and metadata for each ID (see as_r_object)
#' @examples \dontrun{
#' # Get IDs via entrez_fetch:
#' plasticity_ids <- entrez_search("pubmed", "phenotypic plasticity", retmax = 2600)$ids[1:100]
#' plasticity_records <- pubmed_fetch(plasticity_ids)
#' }
pubmed_fetch <- function(ids, ...){
    records <- rentrez::entrez_fetch(db = "pubmed", id = ids, rettype = "xml", retmode = "full", parsed = TRUE, ...)
    XML::xmlToList(records)
}

#' Helper function to split a vector v into list of chunks of chunk_size
#' @param v a vector to be split into chunks
#' @param chunk_size Integer size of the individual chunks
chunker <- function(v, chunk_size){
    split(v, ceiling(seq_along(v)/chunk_size))
}

