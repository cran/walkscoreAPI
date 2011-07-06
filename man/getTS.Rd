\name{getTS}
\alias{getTS}
\title{Transit Score API Call}
\description{A function to perform the basic Transit Score API call.}
\usage{
getTS(x, y, city, state, key)
}

\arguments{
  \item{x}{  longitude of query location (numeric)  }
  \item{y}{  latitude of query location (numeric)  }
  \item{city}{  name of core city where the query location is located (string)  }
  \item{state}{  postal abriviation of query location's state (string)  }
  \item{key}{  your Walk Score API key (string)  }
}
\details{  The Transit Score API call only works in supported cities. Use the functions "checkTSsupport" or "getTScities" to check for support in the city of interest. Also note that calls should use the core city name, even when the query location is technically in a suburb of the core city.
}
\value{Returns an object of class \code{TransitScore}, basically a list of the following elements:
 \item{transitscore }{Transit Score of query location.}
 \item{url }{Link to Walk Score page associated with your query.}
 \item{description }{Qualitative description of query location regarding transit.}
 \item{summary}{Summary of nearby routes and stops.}
}
\references{http://www.walkscore.com/professional/public-transit-api.php}
\author{John Whalen}
\note{Visit www.walkscore.com for information on Walk Score and to obtain an API key}

\seealso{
 \code{\link{getWS}}
}

\examples{
\dontrun{
getTS(-73.98496,40.74807,"New York","NY","your key")
}
}