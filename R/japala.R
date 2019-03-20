#' @title rm(list = ls(all =  T))
#' @description remove all objects included dotted ones
r <- function() {
  all_names <- base::ls(all.names  = T, env = globalenv())
  base::rm(list = all_names, envir = globalenv())
  cat ( '---  deleted ', length(all_names) , 'objects ---', '\n')
  invisible(NULL)
}

# ----------------------------------------------------------
#' @title q 
#' @description quite without saving workspace
q <- function(){
  base::quit(save =  'no') 
}
# ----------------------------------------------------------
#' @title search
#' @description list all attached packages
#' @importFrom dplyr  %>% 
#' @export
p <- function() {
 session <- sessionInfo()
 pkg <- session$otherPkgs
 name <-  unlist(lapply(pkg, '[[', 'Package') %>% lapply(function(x) {ifelse(is.null(x) , NA, x)})) 
 version <- unlist(lapply(pkg, '[[', 'Version') %>% lapply(function(x) {ifelse(is.null(x) , NA, x)}))
 depends <-  unlist(lapply(pkg, '[[', 'Depends') %>% lapply(function(x) {ifelse(is.null(x) , NA, x)}))

 search <- gsub('package:', '' ,search()) 
 position <- seq_along(search)[is.element(search, name)]

 pkg <- data.frame(name, position , version, depends )
 rownames(pkg) <- seq_len(nrow(pkg))
 pkg

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
  .size <- vapply(object, function(x) as.numeric(object.size(x)), FUN.VALUE = numeric(1))
  size <-  vapply(object, object_size, FUN.VALUE = character(1))
  typeof <- vapply(object, typeof, FUN.VALUE = character(1))
  class  <- vapply(object, function(x) paste(class(x), collapse = ' - '), FUN.VALUE = character(1))

  # out
  out <-  data.frame(name, size , class , typeof, stringsAsFactors = FALSE)
  out[order(.size, decreasing = T),]

}  
# ----------------------------------------------------------------
#' @title object size
#' @descriptoion returns object.size(x) formatted with the appropriate  unit of measurement.
#' @export
object_size <- function(x)  {
  object_size <- object.size(x)
  object_size
  size <-  2^c(0, 10, 20,  30 ,  40)            
  unit <-  c('byte', 'Kb', 'Mb', 'Gb', 'Tb') 
  what <- max(seq_len(5)[(object_size/size) > 1])
  paste(round((object_size / size[what]),2) , unit[what])
}
# ----------------------------------------------------------------
#' @title Fresh start  
#' @description Remove all objects and restart R session. All loaded packages will be unloaded
q <- function() { 
  japala:::r()
  cat('---  Restarting R session ---', '\n')
  system("R --quiet ") 
  base::q("no") 

}



#' @title binding 
.onLoad <- function(libname, pkgname) {
  ns <-  asNamespace(pkgname)
  makeActiveBinding(".p", p,  env = ns)
  makeActiveBinding(".q", q,  env = ns)
  makeActiveBinding(".dl", dl, env = ns)
  makeActiveBinding(".dd", dd, env = ns)
  makeActiveBinding(".di", di, env = ns)
  makeActiveBinding(".o", o, env = ns)
  makeActiveBinding(".r", r, env = ns)

  namespaceExport(ns, c( '.p', '.q', '.dd', '.dl', '.di', '.o',  '.r'))
}

andrea_quantide <-  utils::person(given =  'Andrea', 
                                  family = 'Spano', 
                                  email = 'andrea.spano@quantide.com', 
                                  role = c("aut", "cre"))

usethis::use_data(andrea_quantide, overwrite = TRUE) 
