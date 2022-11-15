#' Align signatures
#'
#' Align pairwise across signatures.
#' @param data data frame with id, signatures and grouping
#' @param value symbol for the signature values
#' @param group symbol for grouping
#' @return data frame
#' @export
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#' @importFrom dplyr group_by mutate select pull
#' @importFrom bulletxtrctr sig_align
#' @importFrom stats lag
#' @examples
#' load("data/toolmarks.Rdata")
#' toolmarks %>% group_by(tool, side, size) %>% summarize(angles = length(unique(angle)))
#' tool1AL <- dplyr::filter(toolmarks, tool == "02", side=="A", size=="L")
#' # Align all signatures in one go
#' aligned <- tool1AL %>% mutate(mark_angle = interaction(mark, angle)) %>% sig_align_set(value = signature,  group = mark_angle)
#' aligned %>%
#'   ggplot(aes(x =x, y = aligned, colour = mark_angle)) +
#'   geom_line()
#' wide <- aligned %>% select(-data) %>% pivot_wider(values_from = aligned, names_from = mark_angle)
#' heatmap(cor(wide[,-(1:2)], use="pairwise.complete"))
#'
#' # Align signatures separately by angle
#' long <- tool1AL %>% group_by(angle) %>% nest()
#' long <- long %>% mutate(
#'   aligned = data %>% purrr::map(.f = function(d) {
#'     d %>% sig_align_set(value = signature,  group = mark)
#'   })
#' )
#' long$aligned[[1]] %>% ggplot(aes(x =x, y = aligned, colour = factor(mark))) + geom_line()
#'
#' long <- long %>% select(-data) %>% unnest(col=aligned)
#' long %>%
#'   ggplot(aes(x =x, y = aligned, colour = factor(mark))) +
#'   geom_line() +
#'   facet_grid(angle~., labeller="label_both")
#'
sig_align_set <- function (data, value, group, min.overlap=500) {
  group <- enquo(group)
  value <- enquo(value)
  dlist <- data %>% group_by(!!group) %>% tidyr::nest()
  dlist <- dlist %>% mutate(aligned = data %>% purrr::map(.f = function(d) {
    aligned <- bulletxtrctr::sig_align(dlist$data[[1]] %>%
                                         select(!!value) %>% pull, d %>% select(!!value) %>%
                                         pull, min.overlap = min.overlap)
    #   browser()
    sig1_one <- 0
    idx <- which(!is.na(aligned$lands$sig1))
    if(length(idx) >= 1)
      sig1_one <- idx[1] - 1 # in which position do we have the first non-missing value in sig1?
    aligned$lands %>%
      mutate(x = x - sig1_one,
             ccf = aligned$ccf) %>%
      select(x, aligned = sig2, ccf)
  }))
  long <- dlist %>% unnest(cols = aligned)
  long
}


