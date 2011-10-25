\name{RDF-package}
\alias{RDF-package}
\docType{package}
\title{
RDF reading and writing
}
\description{
Load triples from RDF files into a data.frame. Save triples in a data.frame into RDF files.
}
\details{
\tabular{ll}{
Package: \tab RDF\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-08-30\cr
License: \tab GPL-3\cr
SystemRequirements: \tab libraptor2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Willem Robert van Hage <W.R.van.Hage@vu.nl>
Maintainer: Willem Robert van Hage <W.R.van.Hage@vu.nl>
}
\references{
RDF Primer, \url{http://www.w3.org/TR/rdf-primer/}.\cr
}
\keyword{ package }
\examples{
# create triple data.frame
subject <- c('<http://test/a>', '<http://test/c>')
predicate <- c('<http://test/p>', '<http://test/q>')
object <- c('_:genid1', '"hallo"@nl')
triples <- data.frame(cbind(subject, predicate, object))

# save triple data.frame to RDF file
rdf_save(list(triples=triples,namespaces=c()),
         target="rdftest.ttl", format="turtle",
	 spo=c("subject","predicate","object"),
         namespaces=c('test', '<http://test/>'))

# equivalent, more convenient way to call the function
rdf_save(triples, target="rdftest.ttl", format="turtle",
         namespaces=c('test', '<http://test/>'))

# read triples from RDF file to data.frame
r <- rdf_load("rdftest.ttl")
rdf_save(r, target='filetest.ttl', format='turtle',
         namespaces=c('lop', '<http://semanticweb.cs.vu.nl/poseidon/ns/instances/>'))

# The following statements are now true:
is.list(r)
is.data.frame(r$triples)
is.vector(r$namespaces)

# read triples from RDF over HTTP
r <- rdf_load("http://semanticweb.cs.vu.nl/2009/11/sem/")
rdf_save(r, target='httptest.rdf', "rdfxml",
         namespaces=c('sem', '<http://semanticweb.cs.vu.nl/2009/11/sem/>'))
}

