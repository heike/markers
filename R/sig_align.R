#' Align two surface cross cuts according to maximal correlation
#'
#' @param sig1 vector of first signature
#' @param sig2 vector of second signature
#' @param min.overlap additional parameter passed on to get_ccf
#' @return list consisting of
#'           a) the maximal cross correlation,
#'           b) the lag resulting in the highest cross correlation, and
#'           c) same data frame as input, but y vectors are aligned for
#'              maximal correlation
#'           d) list of a vector of correlations and a vector of lags
#' @export
#' @importFrom assertthat assert_that
#' @importFrom zoo na.trim
#' @importFrom stats cor
sig_align <- function (sig1, sig2, min.overlap = round(0.75 * min(length(sig1),
                                                       length(sig2))))   {
    assert_that(is.numeric(sig1), is.numeric(sig2))
    sig1 <- na.trim(sig1)
    sig2 <- na.trim(sig2)
    n1 <- length(sig1)
    n2 <- length(sig2)
    if (n1 < n2) {
      x <- sig1
      y <- sig2
    }
    else {
      x <- sig2
      y <- sig1
    }
    cors <- bulletxtrctr::get_ccf(x, y, min.overlap = min.overlap)
    lag <- cors$lag[which.max(cors$ccf)]
    if (lag < 0) {
      x <- c(rep(NA, abs(lag)), x)
    }
    if (lag > 0) {
      y <- c(rep(NA, lag), y)
    }
    delta <- length(x) - length(y)
    if (delta < 0)
      x <- c(x, rep(NA, abs(delta)))
    if (delta > 0)
      y <- c(y, rep(NA, delta))
    if (n1 < n2) {
      dframe0 <- data.frame(x = 1:length(x), sig1 = x, sig2 = y)
    }
    else {
      dframe0 <- data.frame(x = 1:length(x), sig1 = y, sig2 = x)
    }
    maxcor <- max(cors$ccf, na.rm = TRUE)
    list(ccf = maxcor, lag = lag, lands = dframe0, cors = cors)
}


