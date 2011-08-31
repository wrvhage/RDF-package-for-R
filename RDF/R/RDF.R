rdf_load <- function(filename_or_URL) {
	spon <- .Call("rdf_load",filename_or_URL, PACKAGE='RDF')
	df <- data.frame(lapply(spon[1:3], as.factor))
	names(df) <- c("subject","predicate","object")
	list(triples = df, namespaces = unique(unlist(spon[4])))
}

rdf_save <- function(triples,target="Rout.rdf",format='rdfxml',namespaces=c()) {
	if (!is.null(triples$triples)) {
		nss = unique(append(triples$namespaces, namespaces))
		.Call("rdf_save",triples$triples,target,format,nss, PACKAGE='RDF')
	} else if (!is.null(triples$results)) {
		nss = unique(append(triples$namespaces, namespaces))
		.Call("rdf_save",triples$results,target,format,nss, PACKAGE='RDF')
	} else if (!is.null(triples)) {
		.Call("rdf_save",triples,target,format,namespaces, PACKAGE='RDF')
	}
}	
