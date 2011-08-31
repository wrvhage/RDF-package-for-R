.First.lib <- function(lib, pkg){
  library.dynam("RDF", package = pkg, lib.loc = lib)
  return(invisible(0))
}

