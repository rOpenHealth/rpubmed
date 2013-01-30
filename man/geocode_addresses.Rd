\name{geocode_addresses}
\alias{geocode_addresses}
\title{Returns a data frame of geocoded addresses with longitude and latitudes
Uses the Google Maps geocode API}
\usage{
  geocode_addresses(addresses, sleeper = 0.33, depth = 3)
}
\arguments{
  \item{addresses}{A character vector of addresses for
  geocoding}

  \item{sleeper}{numeric Number of seconds between calls to
  the geocoding server}

  \item{depth}{integer recursion depth for attempting to
  get coordinates. If the full address fails to get a hit,
  the function is called again with the first line of the
  address removed. The process is repeated depth times
  before returning NAs}
}
\value{
  data frame of addresses, latitudes and longitudes
}
\description{
  Returns a data frame of geocoded addresses with longitude
  and latitudes Uses the Google Maps geocode API
}
\examples{
# get a list of articles pulled from pubmed:
abstracts <- fromJSON("Test/plasticity_abstracts.json")

# Extract affiliated addresses from article metadata:
affil_addresses <- get_article_location_data(abstracts)

# Get coordinates:
coords <- geocode_addresses(affil_addresses, depth = 4)

# plot coordinates on a map:

map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
points(coords$long, coords$lat, col = "red", pch = 20)
}

