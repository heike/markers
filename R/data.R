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
#'   \item{signature}{numeric value the signal height}
#'   \item{tool}{tool ID, value between 1 and 20}
#'   \item{mark}{iteration, value between 1 and 8}
#'   \item{side}{screwdriver side that made the mark, A or B.}
#'   \item{angle}{angle of attack, one of 60, 70, or 80.}
#'   \item{material}{for now all marks are made on lead plates 'Pb'.}
#'   \item{direction}{direction of the toolmark 'Fo'rward/Pull or 'Ba'ckward/Push.}
#'   \item{size}{size of the screwdriver, 'S', 'M', or 'L'}
#' }
#' @source Maria Cuellar, CSAFE collaborative agreement with NIST.
"toolmarks"

#' Wrapper function for toolmarks data set in tmaRks package
#'
#' This function is specific to the`toolmarks` dataset.
#' It unpacks the information stored in the tool ID.
#' This allows us to store the data in a smaller dataset but unpack it for the
#' analysis.
#' @inheritParams utils::data
#' @importFrom tidyr separate_wider_position separate_wider_delim
#' @export
#' @examples
#' data(toolmarks)
#' dim(toolmarks)
data <- function (..., list = character(), package = NULL, lib.loc = NULL,
                  verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) {
  utils::data(..., list=list, package = package, lib.loc = lib.loc,
              verbose=verbose, envir = envir, overwrite = overwrite)

  args <- rlang::ensyms(...)
  datasets <- purrr::map_chr(args, rlang::as_string)

  if ("toolmarks" %in% datasets)
  # now expand it and pass to the outside
  toolmarks <- toolmarks %>%
    separate_wider_delim(.data$TID,
                         names=c("tool_size_side", "direction_angle", "mark"),
                         delim="-", cols_remove=FALSE) %>%
    separate_wider_position(.data$tool_size_side,
                            c(1, tool = 2, size = 1, side = 1)) %>%
    separate_wider_position(.data$direction_angle,
                            c(direction=1, angle=2)) %>%
    mutate(
      tool = as.integer(.data$tool),
      angle = as.integer(.data$angle),
      mark = as.integer(.data$mark),
      direction = ifelse(direction=="F", "Pull",
                         ifelse(direction=="B", "Push", NA))
    ) %>%
    mutate(
      tool = factor(tool),
      side = factor(side),
      size = factor(size, levels=c("S", "L")),
      direction = factor(direction),
      angle = factor(angle),
      mark = factor(mark),
      TID = factor(TID)
    )
  #  makeActiveBinding("toolmarks", toolmarks, topenv())
  assign("toolmarks", toolmarks, envir=envir)
}

