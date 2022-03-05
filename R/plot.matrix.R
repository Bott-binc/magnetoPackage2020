

#' plot.matrix
#'
#' @param x matrix being plotted
#' @param ...
#'
#' @return
#' @export
#'
plot.matrix <-
function (x, ...) {
  pixmap::plot(newPixmapRGB(x, x, x), ...)
}



#' newPixmapRGB
#'
#' Used inside plot.matrix
#'
#' @param red  colour from newPixmapRGB
#' @param green colour from newPixmapRGB
#' @param blue colour from newPixmapRGB
#'
newPixmapRGB <-
function(red=NULL, green=NULL, blue=NULL) {
  pmap.data <- array(data = c(red, green, blue), dim = c(dim(red)[1], dim(red)[2], 3))
  pmap <- pixmap::pixmapRGB(pmap.data, nrow=dim(red)[1], ncol=dim(red)[2],
       bbox=NULL, bbcent=FALSE, cellres=1)

  return(pmap)
}


