#' @title Create interactive shadow text
#' @description
#' The geometry is based on `geom_shadowtext()`.
#' See the documentation for those functions for more details.
#' @param ... see also the parameters of `geom_shadowtext()` of `shadowtext`
#' @export
#' @importFrom rlang check_installed
#' @examples
#' library(ggplot2)
#' library(ggiraph)
#' mtcars$label <- rownames(mtcars)
#' mtcars$tooltip = paste0(
#'    "cyl: ",
#'    mtcars$cyl,
#'    "<br/>",
#'    "gear: ",
#'    mtcars$gear,
#'    "<br/>",
#'    "carb: ",
#'    mtcars$carb
#'  )
#' p <- ggplot(
#'        mtcars,
#'        aes(
#'          x = mpg,
#'          y = wt,
#'          label = label,
#'          color = qsec,
#'          tooltip = tooltip,
#'          data_id = label
#'        )
#'      ) +
#'      geom_shadowtext_interactive(check_overlap = TRUE) +
#'      coord_cartesian(xlim = c(0, 50)) 
#' girafe(ggobj = p, options = list(opts_hover(css = "fill:#FF4C3B;font-style:italic;")))
geom_shadowtext_interactive <- function(...){
  rlang::check_installed('ggiraph', "for `geom_shadowtext_interactive()`.")
  layer_interactive(geom_shadowtext, interactive_geom = GeomInteractiveShadowtext,...)
}


#' @importFrom purrr detect_index
# the internal functions of ggiraph
#' @import ggiraph
layer_interactive <- getFromNamespace("layer_interactive", "ggiraph")
add_default_interactive_aes <- getFromNamespace("add_default_interactive_aes", "ggiraph")
interactive_geom_parameters <- getFromNamespace("interactive_geom_parameters", "ggiraph")
interactive_geom_draw_key <- getFromNamespace("interactive_geom_draw_key", "ggiraph")
IPAR_NAMES <- getFromNamespace("IPAR_NAMES", "ggiraph")
add_interactive_attrs <- getFromNamespace("add_interactive_attrs", "ggiraph")


#' @title ggproto classes for ggiraph
#' @description
#' ggproto classes for ggiraph
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export
GeomInteractiveShadowtext <- ggproto(
  "GeomInteractiveShadowtext",
  GeomShadowtext,
  default_aes = add_default_interactive_aes(GeomShadowtext),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  draw_panel = function(data, panel_params, coord, ..., .ipar = IPAR_NAMES){
    gr <- GeomShadowtext$draw_panel(data, panel_params, coord, ...)
    if (!.check_ipar_params(data)){
      return(gr)
    }
    coords <- coord$transform(data, panel_params)
    ind <- length(gr$children)
    gr$children[[ind]] <- add_interactive_attrs(gr$children[[ind]], coords, ipar=.ipar)
    gr
  }
)

.check_ipar_params <- function(x){
  any(colnames(x) %in% IPAR_NAMES)
}
