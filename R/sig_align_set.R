#' Align signatures
#'
#' Align all signatures in the same group. All signatures in this group are aligned
#' with respect to the first signature in a group.
#' Alignments will vary depending on which signature is used as 'first'.
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
#' load("data/toolmarks.Rdata")
#' toolmarks %>% group_by(tool, side, size) %>% summarize(angles = length(unique(angle)))
#' tool1AL <- dplyr::filter(toolmarks, tool == "02", side=="A", size=="L")
#' # Align all signatures in one go
#' aligned <- tool1AL %>% mutate(mark_angle = interaction(mark, angle)) %>%
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
#' long <- tool1AL %>% group_by(angle) %>% nest()
#' long <- long %>%  mutate(
#'   data = data %>% purrr::map(.f = function(d) {
#'     d %>% sig_align_set(value = signature,  group = mark, min.overlap=500)
#'   })
#' )
#' long$data[[3]] %>% ggplot(aes(x = aligned_x, y = signature, colour = factor(mark))) + geom_line()
#'
#' long <- long %>% unnest(col=data)
#' long %>%
#'   ggplot(aes(x =aligned_x, y = signature, colour = factor(mark))) +
#'   geom_line() +
#'   facet_grid(angle~., labeller="label_both")
#'
sig_align_set <- function (data, value, group, min.overlap) {
  group <- enquo(group)
  value <- enquo(value)
  dlist <- data %>% group_by(!!group) %>% tidyr::nest()
  dlist <- dlist %>% mutate(data = data %>% purrr::map(.f = function(d) {
    # align all signatures to the first
    aligned <- sig_align(dlist$data[[1]] %>%
      select(!!value) %>% pull, d %>% select(!!value) %>%
        pull, min.overlap = min.overlap)
    idx1 <- which(!is.na(aligned$lands$sig1))[1]
    idx2 <- which(!is.na(aligned$lands$sig2))[1]
#browser()
#sig1_one <- idx1 - idx2
    # if(length(idx1) >= 1) { sig1_one <- idx1[1] - 1 # in which position do we have the first non-missing value in sig1?
    # } else {
    #   idx2 <- min(which(!is.na(aligned$lands$sig2)))
    #   if (!is.infinite(idx2)) sig1_one <- -idx2 + 1
    # }
#    aligned$lands %>%
#      mutate(x = x - sig1_one,
#             ccf = aligned$ccf) %>%
#      select(x, aligned = sig2, ccf) %>%
#      slice(1:nrow(d)) # this *should* make the aligned data the same length as the one we started out with
#    browser()

    # this assumes that d is ordered in x
    d %>% mutate(aligned_x = 1:nrow(d) - idx1 + idx2, ccf = aligned$ccf) #
  }))
# browser()
  # now unnest to the longform again
  long <- dlist %>%  unnest(cols = data)
  long
}


