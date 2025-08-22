#' @title Create interactive shadow text
#' @description
#' The geometry is based on `geom_shadowtext()`.
#' See the documentation for those functions for more details.
#' @param ... see also the parameters of `geom_shadowtext()` of `shadowtext`
#' @export
#' @importFrom rlang check_installed
geom_shadowtext_interactive <- function(...){
  rlang::check_installed('ggiraph', "for `geom_shadowtext_interactive()`.")
  layer_interactive(geom_shadowtext, interactive_geom = GeomInteractiveShadowtext,...)
}


#' @importFrom purrr detect_index
# the internal functions of ggiraph
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
    coords <- coord$transform(data, panel_params)
    ind <- length(gr$children)
    gr$children[[ind]] <- add_interactive_attrs(gr$children[[ind]], coords, ipar=.ipar)
    gr
  }
)


