#' @title Check to See if NCO is on the system
#' @description NCO is a fantastic open source tool for working with NetCDF files.
#' It can be downloaded from here: http://nco.sourceforge.net/#Source
#' @return a boolean condition
#' @importFrom sys exec_internal
#' @examples
#' check_nco()
#' @export
#' @family netrc

check_nco = function(){
  m = "Check your NCO install, or download: http://nco.sourceforge.net/#Source."

  tryCatch({
    x = sys::exec_internal("ncks", "--version")
    grepl("version", rawToChar(x$stderr))
  }, error = function(e){
    message(m)
    FALSE
  }, warning = function(w){
    message(m)
    FALSE
  })
}
