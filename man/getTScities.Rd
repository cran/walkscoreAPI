\name{getTScities}
\alias{getTScities}
\title{List Cities With Transit Score Support}
\description{A function to list cities supported by Transit Score.}
\usage{
getTScities(key)
}
\arguments{
  \item{key}{  Your Walk Score API key (string)  }
}
\value{  Prints a list of all cities currently supported by Transit Score.  }
\references{  http://www.walkscore.com/professional/public-transit-api.php  }
\author{  John Whalen  }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }


\seealso{
 \code{\link{checkTSsupport}}
}

\examples{
\dontrun{
getTScities("your key")
}
}