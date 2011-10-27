rdf_load <- function(filename_or_URL) {
	spon <- .Call("rdf_load",filename_or_URL, PACKAGE='RDF')
	df <- data.frame(lapply(spon[1:3], as.factor))
	names(df) <- c("subject","predicate","object")
	list(triples = df, namespaces = unique(unlist(spon[4])))
}

rdf_save <- function(triples,target="Rout.rdf",format='rdfxml',spo=NULL,namespaces=c()) {
	if (!is.null(triples$triples)) {
		if (is.null(spo)) {
			if (length(names(triples$triples)) == 3) {
				spo <- names(triples$triples)
				nss <- unique(append(triples$namespaces, namespaces))
				tr <- expand_qnames(triples$triples,nss)
				.Call("rdf_save",tr,target,format,spo,nss,PACKAGE='RDF')
			} else {
				print("Ambiguous subject predicate and object columns, please specify respective column names with the 'spo' argument.")
			}
		} else {
			nss <- unique(append(triples$namespaces, namespaces))
			tr <- expand_qnames(triples$triples,nss)		
			.Call("rdf_save",tr,target,format,spo,nss,PACKAGE='RDF')
		}
	} else if (!is.null(triples$results)) {
		if (is.null(spo)) {
			if (length(names(triples$results)) == 3) {
				spo <- names(triples$results)
				nss <- unique(append(triples$namespaces, namespaces))
				tr <- expand_qnames(triples$results,nss)
				print(tr)
				.Call("rdf_save",tr,target,format,spo,nss,PACKAGE='RDF')
			} else {
				print("Ambiguous subject predicate and object columns, please specify respective column names with the 'spo' argument.")
			}
		} else {
			nss <- unique(append(triples$namespaces, namespaces))
			tr <- expand_qnames(triples$results,nss)
			.Call("rdf_save",tr,target,format,spo,nss,PACKAGE='RDF')
		}
	} else if (!is.null(triples)) {
		if (is.null(spo)) {
			if (length(names(triples)) == 3) {
				spo <- names(triples)
				tr <- expand_qnames(triples,namespaces)
				.Call("rdf_save",tr,target,format,spo,namespaces,PACKAGE='RDF')
			} else {
				print("Ambiguous subject predicate and object columns, please specify respective column names with the 'spo' argument.")
			}
		} else {
			tr <- expand_qnames(triples,namespaces)
			.Call("rdf_save",tr,target,format,spo,namespaces,PACKAGE='RDF')
		}
	}
}	

expand_qname <- function(q,p) {
	function(a) {
		if (substring(a,1,nchar(q)) == q) {
			x <- paste(gsub(paste('^',q,':',sep=""),substring(p,1,nchar(p)-1),a),'>',sep="")
		} else if (substring(a,1,1) == '<') {
			x <- as.character(a)
		} else if (substring(a,1,1) == '"') {
			x <- as.character(a)
		} else if (substring(a,1,1) == '_') {
			x <- as.character(a)
		} else {
			x <- paste('"',a,'"',sep="")
		}
		x
	}
}

expand_qnames_vec <- function(triples,namespaces) {
        if (!is.null(namespaces)) {
		for (i in seq(1,length(namespaces)-1, by=2)) {
			triples <- sapply(triples, expand_qname(namespaces[i],namespaces[i+1]))
		}
		as.factor(triples)
	} else {
		triples
	}
}

expand_qnames <- function(triples,namespaces) {
	name <- names(triples)
	for (i in seq(1,length(name))) {
		triples[[name[i]]] <- expand_qnames_vec(triples[[name[i]]],namespaces)
	}
	triples
}

guess_type <- function(df,i,spo) {
	if (substring(df[[spo[1]]][i],1,1) != '<') {
		s <- paste('<',df[[spo[1]]][i],'>',sep="")
	} else {
		s <- df[[spo[1]]][i]
	}
	if (substring(df[[spo[2]]][i],1,1) != '<') {
		p <- paste('<',df[[spo[2]]][i],'>',sep="")
	} else {
		p <- df[[spo[2]]][i]
	}
	if (substring(df[[spo[3]]][i],1,1) != '<' &&
            substring(df[[spo[3]]][i],1,1) != '"' && 
            substring(df[[spo[3]]][i],1,1) != '_') {
		if (substring(df[[spo[3]]][i],5,7) == '://' ||
		    substring(df[[spo[3]]][i],4,6) == '://') {
			o <- paste('<',df[[spo[3]]][i],'>',sep="")
		} else if (substring(df[[spo[3]]][i],1,1) != '_') {
			o <- paste('"',df[[spo[3]]][i],'"',sep="")
		}
	} else {
		o <- df[[spo[3]]][i]
	}
	c(s,p,o)
}

guess_types_aux <- function(triples) {
	triples <- data.frame(lapply(triples, as.character), stringsAsFactors=FALSE)
	for (i in seq(1,length(triples[,1]))) {
		spo <- names(triples)
		t <- guess_type(triples,i,spo)
		triples[[spo[1]]][i] <- t[1]
		triples[[spo[2]]][i] <- t[2]
		triples[[spo[3]]][i] <- t[3]
	}
	data.frame(lapply(triples, as.factor), stringsAsFactors=TRUE)
}

guess_types <- function(triples) {
	if (!is.null(triples$triples)) {
		triples$triples <- guess_types_aux(triples$triples)
	} else if (!is.null(triples$results)) {
		triples$results <- guess_types_aux(triples$results)
	} else {
		triples <- guess_types_aux(triples)
	}
	triples
}

