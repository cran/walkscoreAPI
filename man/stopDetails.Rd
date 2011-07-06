\name{stopDetails}
\alias{stopDetails}
\title{Stop Details}
\description{A function to perform the Stop Details API call, which provides additional information about a transit stop of interest.}
\usage{
stopDetails(stopid, key)
}
\arguments{
  \item{stopid}{  The stop ID of the stop of interest. (string)  }
  \item{key}{  your Walk Score API key. (string)  }
}
\details{Stop ID is a unique string of characters used to identfy stops. They can be obtained through the search functions provided in this library.}
\value{Returns an object of class \code{StopDetails}, basically a list of the following elements:
 \item{stopID }{ID of the stop of interest (same as input ID)}
 \item{stopName }{Name of stop of interest.}
 \item{stopLong }{Longitude of stop of interest.}
 \item{stopLat }{Latitude of stop of interest.}
 \item{routeList }{List of route ID's which serve the stop of interest. }
}
\references{  http://www.walkscore.com/professional/public-transit-api.php  }
\author{ John Whalen }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }


\seealso{
 \code{\link{stopSearch}}, \code{\link{networkSearch}}
}

\examples{
\dontrun{
stopDetails("s13993", "your key")
}
}