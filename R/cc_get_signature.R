#' Extract signature from crosscut
#'
#' x3p file of a 3d topological surface is processed at surface
#' crosscut y,
#' @param ccdata crosscut as returned from x3p_crosscut
#' @param span1 The span for the loess fit to get from the profile to the raw
#'          signature
#' @param span2 The span for the loess fit to smooth the raw signature
#' @param resolution numeric value of  the scan resolution, see `x3ptools::x3p_get_scale`.
#' @return data frame
#' @import dplyr
#' @importFrom smoother smth.gaussian
#' @importFrom stats var approx
#' @export
#' @examples
#' if (interactive()) {
#' library(x3ptools)
#' x3p <- x3p_read("~/Documents/CSAFE/Wirecutter/data/Aluminum Wires renamed/T1AW-LI-R1-B01.x3p")
#' line1 <- x3p %>% x3p_extract_profile()
#' line1 <- line1$line_df
#' signature <- cc_get_signature(line1, resolution = x3p %>% x3p_get_scale())
#' }
cc_get_signature <- function (ccdata, span1 = 125, span2 = 5, resolution = 0.645)
{
#  browser()
  x <- y <- value <- raw_sig  <- NULL


  br111 <- ccdata %>%
    group_by(x) %>%
    summarise(
      value = mean(value, na.rm = TRUE)
      ) %>%
    dplyr::select(x, value) %>% as.data.frame()
  dx <- diff(sort(br111$x))
  if (! near(var(dx),0)) { # non zero variance?
    even <- as.data.frame(
      approx(br111$x, br111$value, xout = min(br111$x, na.rm=TRUE)+resolution*0:round(diff(range(br111$x))/resolution))
      )
    br111 <- even
    names(br111) <- c("x", "value")
  }
  br111 <- arrange(br111, x)
  br111 <- br111 %>% mutate(
    raw_sig = value-smoother::smth.gaussian(value, window=span1),
    sig = smoother::smth.gaussian(raw_sig, window=span2)
  )
#  br111 %>%
#    ggplot(aes(x = x))  +
#    geom_line(aes(y = resid125, colour = "125"))

   # br111 %>%
   #   ggplot(aes(x = x))  +
   #   geom_line(aes(y = resid125)) +
   #   geom_line(aes(y = sig3+3, colour = "3")) +
   #   geom_line(aes(y = sig5+6, colour = "5")) +
   #   geom_line(aes(y = sig10+9, colour = "10"))

  br111
}
