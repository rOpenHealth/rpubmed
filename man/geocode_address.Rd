\name{geocode_address}
\alias{geocode_address}
\title{Function to get coordinates from a supplied address
If no match is found, it recursively calls itself on the address minus the first line of the address}
\usage{
geocode_address(address, depth = 3, sleeper = 0)
}
\arguments{
  \item{address}{string}

  \item{depth}{depth integer recursion depth for attempting
  to get coordinates. If the full address fails to get a
  hit, the function is called again with the first line of
  the address removed. The process is repeated depth times
  before returning NAs}

  \item{sleeper}{numeric Number of seconds to pause after
  doing the geocoding}
}
\value{
vector of address, lat, long
}
\description{
Function to get coordinates from a supplied address If no
match is found, it recursively calls itself on the address
minus the first line of the address
}
\examples{
\dontrun{
x <- "Rothamsted Research, Harpenden, Herts AL5 2JQ, UK."
geocode_address(x)
}
}

