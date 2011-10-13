\name{walkshed}
\alias{walkshed}
\title{Return Walk Shed}
\description{A function to preform the "Walking Distance" API Call.}
\usage{
walkshed(x, y, key)
}
\arguments{
  \item{x}{  longitude of query location (numeric)  }
  \item{y}{  latitude of query location (numeric)  }
  \item{key}{  your Walk Score API key (string)  }
}
\details{The "Walking Distance" API call returns the geometry of a polygon which bounds network walking distance from the given origin point.}
\value{Returns an object of class "Walkshed", which is basically a list with the following elements:
 \item{status }{  A code which tells the status of the request. The table of status codes can be found on the Walk Score API site. A status of 1 means a successful request.}
 \item{origin }{  Latitude and longitude of the walkshed center.}
 \item{geometry }{  Geometry type of walkshed.}
 \item{coordinates }{  List of walkshed polygon vertexes, returned as a data frame with two columns.}
 \item{radius }{  Network distance in miles.}
 \item{snappedLong}{  longitude to which the input was snapped to.  }
 \item{snappedLat}{  latitude to which the input was snapped to.  }
}
\references{  http://www.walkscore.com/professional/public-transit-api.php  }
\author{ John Whalen }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }



\examples{
\dontrun{
walkshed(-122.335,47.5815,"your key")
}
}
