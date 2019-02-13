
#' @title ll
#' @description list all objects ls(all.names = T)
ll <- function(){
  base::ls(envir = globalenv(), all.names = TRUE)
}
# ---------------------------------------------------------------
#' @title ls
#' @description list all objects exclude hidden objects ls(all.names = F)
l <- function(){
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

# ----------------------------------------------------------
# ----                  devtools                       -----
# ---------------------------------------------------------- 
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
# --------------------------------------------------------------
#' @title devtools::install()
di <- function() {
  devtools::install(pkg = '.')
  invisible(NULL) 
}


# --------------------------------------------------------------- 
#' @title enriched ls()
#' @export
o <- function() {
  
  # may decide to chenge
  env <- globalenv()
  
  name  <- ls(all.names = TRUE, env = globalenv())
  object <- lapply(name, get, envir = env)
  size <- vapply(object, function(x) as.numeric(object.size(x)), FUN.VALUE = numeric(1))
  typeof <- vapply(object, typeof, FUN.VALUE = character(1))
  class  <- vapply(object, function(x) paste(class(x), collapse = ' - '), FUN.VALUE = character(1))

  # return
  data.frame(name, size , class , typeof, stringsAsFactors = FALSE)
}  









#' @title binding 
.onLoad <- function(libname, pkgname) {
  ns <-  asNamespace(pkgname)
  makeActiveBinding(".ll", ll,  env = ns) 
  makeActiveBinding(".l", l,  env = ns)
  makeActiveBinding(".s", base::search,  env = ns)
  makeActiveBinding(".q", q,  env = ns)
  makeActiveBinding(".dl", dl, env = ns)
  makeActiveBinding(".dd", dd, env = ns)
  makeActiveBinding(".di", di, env = ns)
  makeActiveBinding(".o", o, env = ns)
  namespaceExport(ns, c('.ll','.l', '.s', '.q', '.dd', '.dl', '.o'))
}

andrea_quantide <-  utils::person(given =  'Andrea', 
                                  family = 'Spano', 
                                  email = 'andrea.spano@quantide.com', 
                                  role = c("aut", "cre"))

usethis::use_data(andrea_quantide, overwrite = TRUE) 
