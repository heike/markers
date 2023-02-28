#' Ensure that all files in a vector are correctly named
#'
#' This function runs a list of file names through a pattern check. A warning
#' will be returned in case not all file names pass the check. Offending
#' file names will be shown.
#' Maria: your specific pattern is defined as
#' `"^Sc\\d\\d\\.Pl\\d\\d\\d\\.Ma\\d\\.S[AB]\\.An\\d\\d\\.Pb\\.Dir(Fo|Ba)\\.\\Siz[SML](|.mid)\\.x3p$`
#' @param file (vector of) file name(s)
#' @param pattern format for a study, generally in regular expression. See e.g. \url{https://regexr.com/} for help in creating suitable patterns
#' @param verbose logical value whether function chatters for re-assurance when things works. Defaults to FALSE.
#' @return vector of indices of files that don't pass the check
#' @export
#' @examples
#' files <- dir("data-raw", pattern=".x3p", recursive = TRUE, full.names=FALSE)
#' assert_name_pattern(files, pattern="^T\\d\\d[SML][AB]-[FB][678]0-0[1-8].x3p$")
assert_name_pattern <- function(file, pattern, verbose = FALSE) {
  idx <- grep(pattern=pattern, file)
  no_match <- setdiff(1:length(file), idx)

  if (length(no_match) == 0) {
    # everything is alright. Do we need to say so?
    if (verbose) message(sprintf("all files match pattern <%s>", pattern))
    return(NULL)
  }

  # HH: it would be nice if we could figure out which piece of a regular expression offends the pattern
  warning(sprintf("%d file(s) do not match pattern <%s>:\n'%s'", length(no_match),  pattern, paste(file[no_match], sep=", ", collapse=",\n ")))
  return(no_match)
}


