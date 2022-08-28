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
#' tool1AL <- dplyr::filter(toolmarks, tool == "01", side=="A", size=="L")
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
sig_align_set <- function(data, value, group) {
  group <- enquo(group)
  value <- enquo(value)

  dlist <- data %>% group_by(!!group) %>% tidyr::nest()

  dlist <- dlist %>% mutate(
    aligned = data %>% purrr::map(.f = function(d) {
      aligned <- bulletxtrctr::sig_align(dlist$data[[1]] %>% select(!!value) %>% pull,
                                         d %>% select(!!value) %>% pull)
      aligned$lands %>% mutate(
        lag = aligned$lag
      ) %>% select(x, lag, aligned = sig2)
    })
  )

  long <- dlist %>% unnest(cols = aligned)
  long <- long %>% mutate(
    lag = pmax(lag, 0), x = x - lag) %>% select(-lag)
  long
}



