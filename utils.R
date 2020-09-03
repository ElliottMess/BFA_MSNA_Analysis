

loadInstall_package <- function(package){
  new_pkg_list <- gsub(".*/", "",package)
  new_pkg <- new_pkg_list[!(new_pkg_list %in% installed.packages()[, "Package"])]
  
  if (length(new_pkg)>0){
    install.packages(new_pkg[!grepl("\\/", new_pkg)], dependencies = TRUE)
    remotes::install_github(new_pkg[grepl("\\/", new_pkg)], dependencies = TRUE)
  }
  lapply(new_pkg_list, library, character.only = TRUE)
}

cleaning_chr <- function(string){
  new_string <- stringi::stri_trans_general(string, "Any-ASCII")
}