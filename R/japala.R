#' @title rm(list = ls(all =  T))
#' @description remove all objects included dotted ones
rm <- function() {
  all_names <- base::ls(all.names  = T, env = globalenv())
  base::rm(list = all_names, envir = globalenv())
  cat ( '---  deleted ', length(all_names) , 'objects ---', '\n')
  invisible(NULL)
}

# ---------------------------------------------------------------
#' @title q 
#' @description quite without saving workspace
q <- function(){
  base::quit(save =  'no') 
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
  
  # may decide to change
  env <- globalenv()
  
  name  <- ls(all.names = TRUE, env = globalenv())
  object <- lapply(name, get, envir = env)
  size <- vapply(object, function(x) as.numeric(object.size(x)), FUN.VALUE = numeric(1))
  typeof <- vapply(object, typeof, FUN.VALUE = character(1))
  class  <- vapply(object, function(x) paste(class(x), collapse = ' - '), FUN.VALUE = character(1))

  # return
  data.frame(name, size , class , typeof, stringsAsFactors = FALSE)
}  
# --------------------------------------------------------------- 
#' restart R session
#' @export 
#r <-  function() { system("R"); quit() }
# ----------------------------------------------------------------







#' @title binding 
.onLoad <- function(libname, pkgname) {
  ns <-  asNamespace(pkgname)
  makeActiveBinding(".s", base::search,  env = ns)
  makeActiveBinding(".q", q,  env = ns)
  makeActiveBinding(".dl", dl, env = ns)
  makeActiveBinding(".dd", dd, env = ns)
  makeActiveBinding(".di", di, env = ns)
  makeActiveBinding(".o", o, env = ns)
  makeActiveBinding(".rm", rm, env = ns)

  namespaceExport(ns, c( '.s', '.q', '.dd', '.dl', '.di', '.o',  '.rm'))
}

andrea_quantide <-  utils::person(given =  'Andrea', 
                                  family = 'Spano', 
                                  email = 'andrea.spano@quantide.com', 
                                  role = c("aut", "cre"))

usethis::use_data(andrea_quantide, overwrite = TRUE) 
