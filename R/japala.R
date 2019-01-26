ll_ <- function(){
  ls(envir = globalenv(), all.names = TRUE)
}

l_ <- function(){
  ls(envir = globalenv(), all.names = FALSE)
}







#' @title binding 
.onLoad <- function(libname, pkgname) {
  ns <-  asNamespace(pkgname)
  makeActiveBinding("ll", ll_,  env = ns) 
  makeActiveBinding("l", l_,  env = ns) 
  namespaceExport(ns, c('ll','l'))
}


