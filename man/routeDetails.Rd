\name{routeDetails}
\alias{routeDetails}
\title{Route Details}
\description{A function to perform the Route Details API call, which provides additional information 
about a particular route.
}
\usage{
routeDetails(routeid, key)
}
\arguments{
  \item{routeid}{  Route ID number for the route of interest (string)  }
  \item{key}{  your Walk Score API key (string)  }
}
\details{Route ID is a unique string of characters used to identfy routes. They can be obtained through the search functions provided in this library.
}
\value{Returns an object of class \code{RouteDetails}, basically a list of the following elements:
 \item{routeID }{The route ID of the route of intrest (same as input ID)}
 \item{routeName }{Name of the route of interest}
 \item{routeCatagory }{Mode of transportation associated with this route}
 \item{agency }{Transit agency associated with this route}
 \item{agencyURL }{Agency website}
 \item{routeGeometry }{Geometry of the route linestring, used for mapping the route}
 \item{stopList }{List of stop ID's served by this route.}
}
\references{  http://www.walkscore.com/professional/public-transit-api.php  }
\author{ John Whalen }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }


\seealso{
 \code{\link{networkSearch}},\code{\link{stopDetails}}
}

\examples{
\dontrun{
routeDetails("r415","your key")
}
}