
#' @title ll
#' @description list all objects ls(all.names = T)
ll <- function(){
  base::ls(envir = globalenv(), all.names = TRUE)
}
# ---------------------------------------------------------------
#' @title ls
#' @description list all objects exclude hidden objects ls(all.names = F)
ls <- function(){
  base::ls(envir = globalenv(), all.names = FALSE)
}
# ---------------------------------------------------------------
#' @title q 
#' @description quite without saving workspace
q <- function(){
  base::quit(save =  'no') 
}







#' @title binding 
.onLoad <- function(libname, pkgname) {
  ns <-  asNamespace(pkgname)
  makeActiveBinding(".ll", ll,  env = ns) 
  makeActiveBinding(".l", ls,  env = ns)
  makeActiveBinding(".s", base::search,  env = ns)
 makeActiveBinding(".q", q,  env = ns)
  namespaceExport(ns, c('.ll','.l', '.s', '.q'))
}

andrea_quantide <-  utils::person(given =  'Andrea', 
                                  family = 'Spano', 
                                  email = 'andrea.spano@quantide.com', 
                                  role = c("aut", "cre"))

usethis::use_data(andrea_quantide, overwrite = TRUE) 
