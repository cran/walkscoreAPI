\name{networkSearch}
\alias{networkSearch}
\title{Network Search}
\description{A function to perform the Network Search API call.}
\usage{
networkSearch(x, y, key)
}
\arguments{
  \item{x}{  longitude of query location (numeric)  }
  \item{y}{  latitude of query location (numeric)  }
  \item{key}{  your Walk Score API key (string)  }
}
\details{gives information about all routes and all stops within a mile radius of a query location. This function returns a very data-rich object with details on every stop and every route included in this radius.} 
\value{Returns an object of class \code{NetworkSearch}, which has two elements: first is $routelist, which is a list of objects of class \code{Route}, and second is $stoplist, which is a list of objects of class \code{Stop}.
 \item{routelist }{  List of routeID, route name, route catagory, maintaining agency, agency website, and stops served by the route.  }
 \item{stoplist }{  List of stopID, stop name, stop latitude, stop longitude, and a list of routes which use the stop.  }
%% ...
}
\references{  http://www.walkscore.com/professional/public-transit-api.php  }
\author{ John Whalen }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }

\seealso{
 \code{\link{stopDetails}},\code{\link{routeDetails}}
}

\examples{
\dontrun{
networkSearch(-73.98496,40.74807,"your key")
}
}