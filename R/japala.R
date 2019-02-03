.ll <- function(){
  ls(envir = globalenv(), all.names = TRUE)
}

.ls <- function(){
  ls(envir = globalenv(), all.names = FALSE)
}







#' @title binding 
.onLoad <- function(libname, pkgname) {
  ns <-  asNamespace(pkgname)
  makeActiveBinding(".ll", .ll,  env = ns) 
  makeActiveBinding(".ls", .ls,  env = ns) 
  namespaceExport(ns, c('.ll','.ls'))
}


