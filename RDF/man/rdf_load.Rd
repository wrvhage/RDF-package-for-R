\name{rdf_load}
\alias{rdf_load}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load RDF from file.
}
\description{
This function loads triples from an RDF file indicated by a filename or URL into a data.frame with the columns subject, predicate, and object. Each row represents a triple. The return value of rdf_load is a list containing the triple data.frame and a vector of all namespaces that are defined in the RDF file.
}
\usage{
rdf_load(filename_or_URL)
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename_or_URL}{
%%     ~~Describe \code{url} here~~
  The filename or URL of the RDF file.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
The return value is a list containing a data.frame tagged "triples" and a vector with all namespaces defined in the RDF file tagged "namespaces".
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
RDF Primer, \url{http://www.w3.org/TR/rdf-primer/}.\cr
%% ~put references to the literature/web site here ~
}
\author{
Willem Robert van Hage
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
r <- rdf_load("rdftest.ttl")

is.list(r)
is.data.frame(r$triples)
is.vector(r$namespaces)

}
\keyword{ RDF }
