#' Add icon to the SEM plot
#'
#' @param icon an object class 'rastergrob'
#' @param rm_title logical. When add the icon, should the title be removed? Defalt is TRUE
#'
#' @return a list to be used with ggplot
#' @export
#'
#' @examples
add_icon <- function(icon, rm_title = TRUE){
  require(ggplot2)
  require(grid)

  p_list <- list(
    coord_equal(clip = 'off'),
    annotation_custom(
      icon,
      xmin = 0.05, xmax = 0.15,
      ymin = 0.42, ymax = 0.52
    ),
    if(rm_title) theme(plot.title = element_blank())
  )

  p_list
}
