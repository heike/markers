#' Ensure that all files in a vector are correctly named
#'
#' @param file (vector of) file name(s)
#' @param pattern format for a study, generally in regular expression. See e.g. \url{https://regexr.com/} for help in creating suitable patterns
#' @export
#' @examples
#' path <- "~/Documents/projects/toolmarks/data/x3p_files" # HH sorry for using absolute paths, Maria, you will need to exchange that with the path to the x3pfiles on your machine
#' files <- dir(path, pattern="x3p", recursive = TRUE, full.names=FALSE)
#' assert_name_pattern(files, pattern="^Sc\\d\\d\\.Pl\\d\\d\\d\\.Ma\\d\\.S[AB]\\.An\\d\\d\\.Pb\\.Dir(Fo|Ba)\\.\\Siz[SML]\\.x3p$")
assert_name_pattern <- function(file, pattern, verbose = FALSE) {
  idx <- grep(pattern=pattern, file)
  no_match <- setdiff(1:length(file), idx)

  if (length(no_match) == 0) {
    # everything is alright. Do we need to say so?
    if (verbose) message(sprintf("all files match pattern <%s>", pattern))
    return(NULL)
  }

  warning(sprintf("%d file(s) do not match pattern <%s>:\n'%s'", length(no_match),  pattern, paste(file[no_match], sep=", ", collapse=",\n ")))
  return(no_match)
}


