##' a shadow version of geom_text
##'
##'
##' @title geom_shadowtext
##' @param mapping aesthetic mapping
##' @param data the data to be displayed
##' @param stat statistical transformation
##' @param position position adjustment
##' @param ... additional parameter
##' @param parse whether parse text as expression
##' @param nudge_x horizontal adjustment of text
##' @param nudge_y vertical adjustment of text
##' @param check_overlap whether check overlap
##' @param na.rm whether remove NA values
##' @param show.legend whether show legend
##' @param inherit.aes whether inherit aes from ggplot
##' @return layer
##' @importFrom ggplot2 layer
##' @importFrom ggplot2 position_nudge
##' @author guangchuang yu
##' @export
##' @examples
##' library(ggplot2)
##' d <- data.frame(x = rnorm(3), y=rnorm(3), label = c('hello', 'world', '!!!'))
##' ggplot(d, aes(x,y)) + geom_shadowtext(aes(label=label, color=label), bgcolor='firebrick')
##' @export
geom_shadowtext <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            parse = FALSE,
                            nudge_x = 0,
                            nudge_y = 0,
                            check_overlap = FALSE,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
        }

        position <- position_nudge(nudge_x, nudge_y)
    }

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomShadowText,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            parse = parse,
            check_overlap = check_overlap,
            na.rm = na.rm,
            ...
        ),
        check.param = FALSE
    )
}

##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 draw_key_text
##' @importFrom ggplot2 Geom
GeomShadowText <- ggproto("GeomShadowText", Geom,
                          required_aes = c("x", "y", "label"),

                          default_aes = aes(
                              colour = "white", size = 3.88, angle = 0, hjust = 0.5,
                              vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                              bg.color = "black", bg.r = 0.1
                          ),

                          draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                na.rm = FALSE, check_overlap = FALSE) {
                              lab <- data$label
                              if (parse) {
                                  lab <- parse(text = as.character(lab))
                              }

                              data <- coord$transform(data, panel_params)
                              if (is.character(data$vjust)) {
                                  data$vjust <- compute_just(data$vjust, data$y)
                              }
                              if (is.character(data$hjust)) {
                                  data$hjust <- compute_just(data$hjust, data$x)
                              }

                              shadowtextGrob(
                                  lab,
                                  data$x, data$y, default.units = "native",
                                  hjust = data$hjust, vjust = data$vjust,
                                  rot = data$angle,
                                  bg.color = alpha(data$bg.color, data$alpha),
                                  bg.r = data$bg.r,
                                  gp = gpar(
                                      col = alpha(data$colour, data$alpha),
                                      fontsize = data$size * .pt,
                                      fontfamily = data$family,
                                      fontface = data$fontface,
                                      lineheight = data$lineheight
                                  ),
                                  check.overlap = check_overlap
                              )
                          },

                          draw_key = draw_key_text
                          )

compute_just <- getFromNamespace("compute_just", "ggplot2")
just_dir <- getFromNamespace("just_dir", "ggplot2")



##' create and draw text
##'
##'
##' @title shadowtextGrob
##' @param label text labels
##' @param x x position
##' @param y y position
##' @param just The justification of the text, can be 'left', 'right', 'center', 'bottom' and 'top'
##' @param hjust horizontal adjustment
##' @param vjust vertical adjustment
##' @param rot angle to rotate the text
##' @param check.overlap whether check for and omit overlapping text
##' @param default.units default unit of x and y
##' @param name identifier
##' @param gp gpar object
##' @param vp viewport object
##' @param bg.color background color of shadow text
##' @param bg.r background ratio of shadow text
##' @return grob object
##' @importFrom grid textGrob
##' @importFrom grid unit
##' @importFrom grid gpar
##' @importFrom grid gList
##' @importFrom grid gTree
##' @importFrom grid is.unit
##' @export
##' @author guangchuang yu
shadowtextGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
                           default.units = "npc", name = NULL, gp = gpar(col="white"), vp = NULL,
                           bg.color = "black", bg.r = 0.1) {

    upperGrob <- textGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                          vjust = vjust, rot = rot, default.units = default.units,
                          check.overlap = check.overlap, name = name, gp = gp, vp = vp)

    gp$col <- bg.color
    ovp <- vp

    theta <- seq(pi/8, 2*pi, length.out=16)
    char <- substring(label[1], 1, 1)
    r <- bg.r[1]

    bgList <- lapply(theta, function(i) {
        vp <- ovp
        if (!is.unit(x))
            x <- unit(x, default.units)
        if (!is.unit(y))
            y <- unit(y, default.units)

        x <- x + unit(cos(i) * r, "strwidth", data = char)
        y <- y + unit(sin(i) * r, "strheight", data = char)
        textGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                 vjust = vjust, rot = rot, default.units = default.units,
                 check.overlap = check.overlap, name = name, gp = gp, vp = vp)
    })

    bgGrob <- do.call(gList, bgList)
    grobs <- gList(bgGrob, upperGrob)
    gTree(children = grobs)
}

##' @rdname shadowtextGrob
##' @param draw whether draw the grob
##' @importFrom grid grid.draw
##' @export
grid.shadowtext <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
                           default.units = "npc", name = NULL, gp = gpar(col="white"), vp = NULL,
                           bg.color = "black", bg.r = 0.1, draw = TRUE) {
    stg <- shadowtextGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                          vjust = vjust, rot = rot, default.units = default.units,
                          check.overlap = check.overlap, name = name, gp = gp, vp = vp,
                          bg.color = bg.color, bg.r = bg.r)
    if (draw) grid.draw(stg)
    invisible(stg)
}

##' @importFrom grid gpar
##' @export
grid::gpar
