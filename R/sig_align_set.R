#' Align signatures
#'
#' Align all signatures in the same group. All signatures in this group are aligned
#' with respect to the first signature in a group.
#' Alignments will vary depending on which signature is used as 'first'.
#' TODO: check on resolution
#' IMPORTANT: the value vectors have to be in the same order for each group and
#' taken on the same, equi-distant grid (no missing values!)
#' @param data data frame with id, signatures and grouping variable
#' @param value symbol for the signature values
#' @param group symbol (variable) for grouping
#' @param min.overlap value passed on to `get_ccf`, see `bulletxtrctr::get_ccf`.
#' @return data frame: the returned data frame has the same number of observations as the input,
#' but has the additional variables `aligned_x` and `ccf`. Note that `aligned_x` is reported in integer values.
#' For a conversion to the physical extent, scale by the resolution of the scan.
#' @export
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#' @importFrom dplyr group_by mutate select pull lag
#' @importFrom bulletxtrctr sig_align
#' @examples
#' library(dplyr)
#' data(toolmarks)
#' names(toolmarks)
#' tool2AL <- dplyr::filter(toolmarks, tool == 2, side=="A", size=="L")
#' # Align all signatures in one go
#' aligned <- tool2AL %>% mutate(mark_angle = interaction(mark, angle)) %>%
#'   sig_align_set(value = signature,  group = mark_angle, min.overlap = 500)
#'
#' # Visualize the results:
#' library(ggplot2)
#' aligned %>%
#'   ggplot(aes(x = aligned_x, y = signature, colour = mark_angle)) +
#'   geom_line()
#' # HH: mark 4.60 is still travelling in reversed direction
#'
#' wide <- aligned %>% select(aligned_x, signature, mark_angle) %>%
#'    tidyr::pivot_wider(values_from = signature, names_from = mark_angle)
#' heatmap(cor(wide %>% select(-aligned_x), use="pairwise.complete"))
#'
#' # Align signatures separately by angle
#' long <- tool2AL %>% group_by(angle) %>% tidyr::nest()
#' long <- long %>%  mutate(
#'   data = data %>% purrr::map(.f = function(d) {
#'     d %>% sig_align_set(value = signature,  group = mark, min.overlap=500)
#'   })
#' )
#' long$data[[3]] %>% ggplot(aes(x = aligned_x, y = signature, colour = factor(mark))) + geom_line()
#'
#' long <- long %>% tidyr::unnest(col=data)
#' long %>%
#'   ggplot(aes(x =aligned_x, y = signature, colour = factor(mark))) +
#'   geom_line() +
#'   facet_grid(angle~., labeller="label_both")
#'
sig_align_set <- function (data, value, group, min.overlap=500) {
  group <- enquo(group)
  value <- enquo(value)

  # if("x" %in% names(data)) {
  #   diffx <- data$x %>% unique() %>% sort() %>% diff()
  #   cat("For a horizontal crosscut we only have one value here.")
  # }
  glevels <- data %>% dplyr::select(!!group) %>% pull %>% levels()
  first <- glevels[1]

  dlist <- data %>% group_by(!!group) %>% tidyr::nest()
  firstidx <- which(dlist %>% select(!!group) %>% pull ==first)
  dlist <- dlist %>% mutate(data = data %>% purrr::map(.f = function(d) {
    # align all signatures to the first group level
    aligned <- sig_align(dlist$data[[firstidx]] %>%
      select(!!value) %>% pull, d %>% select(!!value) %>%
        pull, min.overlap = min.overlap)
    idx1 <- which(!is.na(aligned$lands$sig1))[1]
    idx2 <- which(!is.na(aligned$lands$sig2))[1]

    # we assume that d is ordered in x
    d %>% mutate(
      aligned_x = 1:nrow(d) - idx1 + idx2,
      ccf = aligned$ccf,
      lag = idx2 - idx1) #
  }))

  # now unnest to the longform again
  long <- dlist %>%  unnest(cols = data)
  long
}


