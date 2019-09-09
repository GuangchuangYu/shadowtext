
##' shadown text element for ggplot theme system
##'
##' 
##' @title element_shadowtext
##' @param family Font family
##' @param face Font face ("plain", "italic", "bold", "bold.italic")
##' @param colour text colour
##' @param size text size in pts
##' @param hjust horizontal justification (in [0, 1])
##' @param vjust vertical justification (in [0, 1])
##' @param angle text angle
##' @param lineheight line height
##' @param color aliase to colour
##' @param margin margins around the text, see also 'margin()' for more details
##' @param debug if 'TRUE', aids visual debugging by drawing a solic rectangle behind the complete text area, and a point where each label is anchored.
##' @param inherit.blank whether inherit 'element_blank'
##' @return element_shadowtext object
##' @export
##' @author Guangchuang Yu and xmarti6@github
element_shadowtext <- function (family = NULL, face = NULL, colour = NULL, size = NULL,
                                hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
                                color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE) {
    if (!is.null(color))
        colour <- color
    ## Create a new "subclass" from "element_text"
    structure(list(family = family, face = face, colour = colour,
                   size = size, hjust = hjust, vjust = vjust, angle = angle,
                   lineheight = lineheight, margin = margin, debug = debug,
                   inherit.blank = inherit.blank), 
              class = c("element_shadowtext", "element_text",  "element"))  
}

##' @importFrom ggplot2 element_grob
##' @method element_grob element_shadowtext
##' @export
element_grob.element_shadowtext <- function (element, label = "", x = NULL, y = NULL, family = NULL,
                                             face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL,
                                             angle = NULL, lineheight = NULL, margin = NULL, margin_x = FALSE,
                                             margin_y = FALSE, ...) {
    if (is.null(label))                                                              
        return(zeroGrob())                                                           
    vj <- vjust %||% element$vjust                                                   
    hj <- hjust %||% element$hjust                                                   
    margin <- margin %||% element$margin                                             
    angle <- angle %||% element$angle %||% 0                                         
    gp <- gpar(fontsize = size, col = colour, fontfamily = family,                   
               fontface = face, lineheight = lineheight)                                    
    element_gp <- gpar(fontsize = element$size, col = element$colour,                
                       fontfamily = element$family, fontface = element$face,                        
                       lineheight = element$lineheight)                                             
    shadow.titleGrob(label, x, y, hjust = hj, vjust = vj, angle = angle,
                     gp = modify_list(element_gp, gp), margin = margin, margin_x = margin_x,      
                     margin_y = margin_y, debug = element$debug)                                  
}

##' @importFrom grid rectGrob
##' @importFrom grid pointsGrob
shadow.titleGrob <- function (label, x, y, hjust, vjust, angle = 0, gp = gpar(),                     
                              margin = NULL, margin_x = FALSE, margin_y = FALSE, debug = FALSE) {                                                                                
    if (is.null(label))                                                          
        return(zeroGrob())                                                       
    grob_details <- shadow.title_spec(label, x = x, y = y, hjust = hjust,   #<<<<            
                                      vjust = vjust, angle = angle, gp = gp, debug = debug)                    
    add_margins(grob = grob_details$text_grob, height = grob_details$text_height,
                width = grob_details$text_width, gp = gp, margin = margin,               
                margin_x = margin_x, margin_y = margin_y)                                
} 


shadow.title_spec <- function (label, x, y, hjust, vjust, angle, gp = gpar(), debug = FALSE) {
    if (is.null(label))                                                                  
        return(zeroGrob())                                                               
    just <- rotate_just(angle, hjust, vjust)                                             
    n <- max(length(x), length(y), 1)                                                    
    x <- x %||% unit(rep(just$hjust, n), "npc")                                          
    y <- y %||% unit(rep(just$vjust, n), "npc")                                          
    #text_grob <- textGrob(label, x, y, hjust = hjust, vjust = vjust,   #<<<<TARGET                   
                                        #    rot = angle, gp = gp)   
    text_grob <- shadowtextGrob(label, x, y, hjust = hjust, #<<<<TARGET_EDITED
                                vjust = vjust, rot = angle, gp = gp)
    
		
    descent <- font_descent(gp$fontfamily, gp$fontface, gp$fontsize, gp$cex)
                                        #The "grobheight/width" are only extracted from a "textGrob" but not from
                                        #a "gTree" which is a list of textGrobs generated by "shadowtext". 
                                        #It is enough if we get the height/width from the 1st element textGrob from gTree. 
                                        #To access textGrobs from gTree we use "$children". 
                                        #text_height <- unit(1, "grobheight", text_grob) + abs(cos(angle/180 *     #<<<<EDITED            
    text_height <- unit(1, "grobheight", text_grob) + abs(cos(angle/180 *                 
                                                              pi)) * descent                                                                   
                                        #text_width <- unit(1, "grobwidth", text_grob) + abs(sin(angle/180 *      #<<<<EDITED            
    text_width <- unit(1, "grobwidth", text_grob) + abs(sin(angle/180 *  
                                                            pi)) * descent                                                                   
    if (isTRUE(debug)) {                                                                 
        children <- gList(rectGrob(gp = gpar(fill = "cornsilk",                          
                                             col = NA)), pointsGrob(x, y, pch = 20, gp = gpar(col = "gold")),             
                          text_grob)                                                                   
    }                                                                                    
    else {                                                                               
        children <- gList(text_grob)                                                     
    }                                                                                    
    list(text_grob = children, text_height = text_height, text_width = text_width)       
}

"%||%" <- getFromNamespace("%||%", "ggplot2")
zeroGrob <- getFromNamespace("zeroGrob", "ggplot2")
modify_list <- getFromNamespace("modify_list", "ggplot2")
add_margins <- getFromNamespace("add_margins", "ggplot2")
rotate_just <- getFromNamespace("rotate_just", "ggplot2")
font_descent <- getFromNamespace("font_descent", "ggplot2")
