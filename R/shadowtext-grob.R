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

    if (is.null(bg.color))
        return(upperGrob)


    gp$col <- bg.color

    theta <- seq(pi/8, 2*pi, length.out=16)
    char <- substring(label[1], 1, 1)
    r <- bg.r[1]

    bgList <- lapply(theta, function(i) {
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
