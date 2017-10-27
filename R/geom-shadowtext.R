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
                      inherit.aes = TRUE)
{
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
    bgcolor = "black"
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
      bgcolor = alpha(data$bgcolor, data$alpha),
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

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

##' @importFrom grid textGrob
##' @importFrom grid unit
##' @importFrom grid gpar
##' @importFrom grid gList
##' @importFrom grid gTree
shadowtextGrob <- function (label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
    just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
    default.units = "npc", name = NULL, gp = gpar(), vp = NULL,
    bgcolor = NULL) {

    upperGrob <- textGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                        vjust = vjust, rot = rot, default.units = default.units,
                        check.overlap = check.overlap, name = name, gp = gp, vp = vp)

    if (is.null(bgcolor))
        return(upperGrob)

    xr <- diff(range(as.numeric(x))) / 200
    yr <- diff(range(as.numeric(y))) / 200

    if (xr == 0) xr <- 0.002
    if (yr == 0) yr <- 0.002

    gp$col <- bgcolor
    theta <- seq(pi/8, 2*pi, length.out=16)
    ovp <- vp
    char <- substring(label, 1, 1)

    bgList <- lapply(theta, function(i) {
        vp <- ovp
        ci <- cos(i) * xr
        si <- sin(i) * yr
        x <- x + sapply(seq_along(ci), function(i) unit(ci[i], "strwidth", data = char[i]))
        y <- y + sapply(seq_along(si), function(i) unit(si[i], "strheight", data = char[i]))
        textGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                 vjust = vjust, rot = rot, default.units = default.units,
                 check.overlap = check.overlap, name = name, gp = gp, vp = vp)
    })

    bgGrob <- do.call(gList, bgList)
    grobs <- gList(bgGrob, upperGrob)
    gTree(children = grobs)
}


