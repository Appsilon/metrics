add_opacity <- function(color, opacity = NULL) {
  if (!is.null(opacity)) {
    return(paste0(color, opacity))
  }
  return(color)
}
