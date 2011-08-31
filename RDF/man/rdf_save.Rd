\name{rdf_save}
\alias{rdf_save}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Save RDF to file.
}
\description{
This function saves triples from a data.frame with the columns subject, predicate, and object to an RDF file indicated by a filename. Each row in the data.frame represents a triple.
}
\usage{
rdf_save(triples,target="Rout.rdf",format='rdfxml',namespaces=c())
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
\item{triples}{
  A data.frame containing the columns "subject", "predicate", and "object". Each row represents a triple. The values in "subject" and "predicate" should be URIs enveloped by angle brackets ("<" ">"), e.g. '<http://test/x123>', encoded as character factors. The values of "object" can be enveloped URIs or strings with optional datatype or language tags, also encoded as character factors. For example, '"persona"', '"hond"@nl', or '"12.4"^^<http://www.w3.org/2001/XMLSchema#decimal>'.
}
\item{target}{
  The filename of the target RDF file.
}
\item{format}{
  The shorthand name of the RDF syntax to be used. All shorthands supported by libraptor2, see: http://librdf.org/raptor/, are also supported by rdf_save.
}
\item{namespaces}{
  A vector containing pairs of namespaces and their shorthands to be used for shortening URIs in the file. For example, c('rdf', '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>', 'dc', '<http://purl.org/dc/elements/1.1/>').
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  rdf_save returns NULL.
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
# create triple data.frame
subject <- c('<http://test/a>', '<http://test/c>')
predicate <- c('<http://test/p>', '<http://test/q>')
object <- c('_:genid1', '"hallo"@nl')
triples <- data.frame(cbind(subject, predicate, object))

# save triple data.frame to RDF file
rdf_save(list(triples=triples,namespaces=c()),
         target="rdftest.rdf", format="rdfxml",
         namespaces=c('test', '<http://test/>'))

# equivalent, more convenient way to call the function
rdf_save(triples, target="rdftest.rdf", format="rdfxml",
         namespaces=c('test', '<http://test/>'))

}
\keyword{ RDF }
