#' Toolmarks data
#'
#' A readable link to the doi for a data repo would be much better, here.
#'
#' @format ## `toolmarks`
#' A data frame with 38,6712 rows and 13 columns:
#' \describe{
#'   \item{x}{numeric value of the x location in ???}
#'   \item{y}{numeric value of the y location in ???}
#'   \item{value}{numeric value of the surface height in ???}
#'   \item{pred}{numeric value - not sure what it's for}
#'   \item{signature}{numeric value the signal height}
#'   \item{tool}{tool ID, value between 1 and 20}
#'   \item{plate}{physical plate the mark is on}
#'   \item{mark}{iteration, value between 1 and 8}
#'   \item{side}{screwdriver side that made the mark, A or B.}
#'   \item{angle}{angle of attack, one of 60, 70, or 80.}
#'   \item{material}{for now all marks are made on lead plates 'Pb'.}
#'   \item{direction}{direction of the toolmark 'Fo'rward or 'Ba'ckward.}
#'   \item{size}{size of the screwdriver, 'S', 'M', or 'L'}
#' }
#' @source Maria Cuellar, CSAFE collaborative agreement with NIST.
"toolmarks"

