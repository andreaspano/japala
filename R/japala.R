
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
# ---------------------------------------------------------------
#' @title lst 
#' @export 
lst <- function() {

  out <- tibble::tibble(obj = ll()) 
  return(out)

}
# --------------------------------------------------------------- 
#' @title devtools::load_all('.')
dl <- function() {
  devtools::load_all(path = '.')
  invisible(NULL) 
}  
# --------------------------------------------------------------- 
#' @title devtools::document()
dd <- function() {
  devtools::document(pkg = '.')
  invisible(NULL) 
}  

#z <- sapply(ls(), function()
#object.size(get(x, envir = globalenv())))







#' @title binding 
.onLoad <- function(libname, pkgname) {
  ns <-  asNamespace(pkgname)
  makeActiveBinding(".ll", ll,  env = ns) 
  makeActiveBinding(".l", ls,  env = ns)
  makeActiveBinding(".s", base::search,  env = ns)
  makeActiveBinding(".q", q,  env = ns)
  makeActiveBinding(".dl", dl, env = ns)
  makeActiveBinding(".dd", dd, env = ns)

  namespaceExport(ns, c('.ll','.l', '.s', '.q', '.dd'))
}

andrea_quantide <-  utils::person(given =  'Andrea', 
                                  family = 'Spano', 
                                  email = 'andrea.spano@quantide.com', 
                                  role = c("aut", "cre"))

usethis::use_data(andrea_quantide, overwrite = TRUE) 
