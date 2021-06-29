#' Path to cache
#' @return path
#' @export
#' @family cache

geo_path <- function(){
  "/Volumes/Transcend/ngen"
}

.getExtension <- function(file){
  ext <- strsplit(basename(file), split="\\.")[[1]]
  return(ext[length(ext)])
}

.getSize <- function(file){
  fs <- file.size(file)
  ifelse(!is.na(fs), round(fs/10 ^ 6, 3), NA)
}

#' Manage cached files
#' @export
#' @param pattern pattern to look for using list.files
#' @export
#' @importFrom dplyr tibble
#' @family cache

geo_cache_list <- function(pattern = NULL) {
  ts = tibble(fullname = list.files(geo_path(), pattern = pattern, ignore.case = TRUE,
             recursive = TRUE, full.names = TRUE))
  ts$subdir   = gsub(geo_path(), "", dirname(ts$fullname))
  ts$basename = basename(ts$fullname)
  ts$ext      = .getExtension(file = ts$fullname)
  ts$size_mb  = .getSize(ts$fullname)
  ts
}

