\name{stopSearch}
\alias{stopSearch}
\title{Stop Search}
\description{A function to preform the Stop Search API Call.}
\usage{
stopSearch(x, y, key)
}
\arguments{
  \item{x}{  longitude of query location (numeric)  }
  \item{y}{  latitude of query location (numeric)  }
  \item{key}{  your Walk Score API key (string)  }
}
\details{The Stop Search API Call returns the sixteen closest stops to a query location which service unique routes.}
\value{Returns a list of objects of class "Stop2", which are basically lists with the following elements:
 \item{stopID }{  A unique ID for the particular stop, which can be used to get additional info about that stop.}
 \item{stopName }{  Name of the particular stop.}
 \item{stopDistance }{  Distance to the query location, in miles.}
 \item{stopLong }{  Longitude of the particular stop.}
 \item{stopLat }{  Latitude of the particular stop.}
 \item{routeDetails }{  Object of class "RouteDetails", which lists the route id, name, catagory, and agency of the orute served at the stop. }
}
\references{  http://www.walkscore.com/professional/public-transit-api.php  }
\author{ John Whalen }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }

\seealso{
 \code{\link{stopDetails}}, \code{\link{networkSearch}}
}

\examples{
\dontrun{
stopSearch(-73.98496,40.74807,"your key")
}
}
