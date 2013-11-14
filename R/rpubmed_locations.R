# Tools for geocoding addresses affiliated with Pubmed Records
# Geocoder still needs some work - Not a good enough hit rate...

#' Returns a data frame of geocoded addresses with longitude and latitudes
#' Uses the Google Maps geocode API
#' @export 
#' @param addresses A character vector of addresses for geocoding
#' @param sleeper numeric Number of seconds between calls to the geocoding server
#' @param depth integer recursion depth for attempting to get coordinates. If the full address fails to get a hit, the function is called again with the first line of the address removed. The process is repeated depth times before returning NAs
#' @return data frame of addresses, latitudes and longitudes
#' @examples \dontrun{
#'  # get a list of articles pulled from pubmed:
#' abstracts <- fromJSON("Test/plasticity_abstracts.json")
#' 
#' # Extract affiliated addresses from article metadata:
#' affil_addresses <- get_article_location_data(abstracts)
#' 
#' # Get coordinates:
#' coords <- geocode_addresses(affil_addresses, depth = 4)
#' 
#' # plot coordinates on a map:
#' 
#' map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
#' points(coords$long, coords$lat, col = "red", pch = 20)
#' }


geocode_addresses <- function(addresses, sleeper = 0.33, depth = 3){
    coords <- t(sapply(addresses, 
                       function(addr){
                           as.numeric(geocode_address(addr, depth = depth, sleeper = sleeper))
                       }))
    data.frame(address = row.names(coords), lat = coords[,1], long = coords[,2])
}


#' Extracts addresses of affiliated departments from Pubmed metadata
#' email addresses are cleaned out.
#' @export 
#' @param abstracts A list of Pubmed records. e.g. from fetch_in_chunks()
#' @return character vector of addresses
#' @examples \dontrun{
#' # Extract affiliated addresses from article metadata:
#' affil_addresses <- get_article_location_data(abstracts)
#' }

get_article_location_data <- function(abstracts){
    raw_locations <- as.character(do.call(rbind,
                            lapply(abstracts,
                                   function(x) x$MedlineCitation$Article$Affiliation)))
    locations <- gsub(pattern= "[[:alnum:][:punct:]]+@+[[:alnum:][:punct:]]+", "", raw_locations)
    locations
}

#' Function to get coordinates from a supplied address
#' If no match is found, it recursively calls itself on the address minus the first line of the address
#' @export
#' @param address string 
#' @param depth depth integer recursion depth for attempting to get coordinates. If the full address fails to get a hit, the function is called again with the first line of the address removed. The process is repeated depth times before returning NAs
#' @param sleeper numeric Number of seconds to pause after doing the geocoding
#' @return vector of address, lat, long
#' 
#' @examples \dontrun{
#' x <- "Rothamsted Research, Harpenden, Herts AL5 2JQ, UK."
#' geocode_address(x)
#' }

geocode_address <- function(address, depth = 3, sleeper = 0){
    coords <- geocode(address)
    Sys.sleep(sleeper)
    if(!is.null(names(coords)) & is.na(coords[1]) & depth > 0){
        address <- sub(pattern="[[:alnum:][:punct:][:space:]][^,]*, ?", "", address)
        return(geocode_address(address, depth = depth -1))
    }
    coords
}


#' Helper function for geocode_address
geocode <- function(address){
    gcStr <- gsub(' ','%20', address) #Encode URL Parameters
    #Open Connection
    connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr, sep="") 
    con <- url(connectStr)
    tryCatch({
        data.json <- fromJSON(paste(readLines(con), collapse=""))
        close(con)
        #Flatten the received JSON
        data.json <- unlist(data.json)
        lat <- data.json["results.geometry.location.lat"]
        lng <- data.json["results.geometry.location.lng"]
        gcodes <- c(lat, lng)
        names(gcodes) <- c("Lat", "Lng")
        #print(paste(address, gcodes$Lat, gcodes$Lng))
        return (gcodes)    
    }, error = function(e) return(c(NA,NA)))
}

