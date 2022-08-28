#' Show surface matrix as a plot
#'
#' More details to come
#' @param x3p scan in x3p format
#' @param dimension 2 or 3
#' @param width size of window
#' @param scale factor to scale the original matrix. Large matrices might crash your session.
#' @param texture texture object. Create one with `rayshader::create_texture` or use one of the pre-made one (e.g. "bw", "imhof1", "imhof2", "imhof3", "unicorn", "desert")
#' @importFrom rayshader plot_map resize_matrix
#' @importFrom rgl plot3d
#' @export
x3p_image <- function(x3p, dimension = 3, width = 400, scale = 0.25, texture = "bw") {
  stopifnot("x3p" %in% class(x3p))
  scan_mat <- x3p$surface.matrix
  scan_small = rayshader::resize_matrix(scan_mat, scale)


  base <- scan_small %>%
    sphere_shade(texture = texture,
                 zscale=.01)

  p <- NULL
  if (dimension == 2) p <- base %>%  plot_map()
  if (dimension == 3) {
    dims <- dim(scan_small)
    window <- c(width, round(width)*dims[2]/dims[1])
    p <- base %>%  plot_3d(scan_small, windowsize = window, theta=40,  phi=50, zoom=0.4,  fov=90)
  }
  return(p)
}
