#' @title Function \code{mytheme}
#' @description ggplot2 theme
#' @export
#' @return ggplot2 theme
mytheme = function(){
  theme(axis.text.x = element_text(family = "Helvetica", colour = 'black', angle = -80, hjust = 0),
                  panel.background = element_rect(fill='white'),
                  panel.border = element_rect(color="black", fill = NA),
                  panel.grid.major = element_line(color="lightgray"),
                  text = element_text(family = "Helvetica", colour= "black"))
}

#' @title Function \code{mytheme_straight}
#' @description ggplot2 theme
#' @export
#' @return ggplot2 theme
mytheme_straight = function(){
  theme(axis.text.x = element_text(family = "Helvetica", colour = 'black'),
                  panel.background = element_rect(fill='white'),
                  panel.border = element_rect(color="black", fill = NA),
                  panel.grid.major = element_line(color="lightgray"),
                  text = element_text(family = "Helvetica", colour= "black"))
}

#' @title Function \code{mytheme_pub}
#' @description ggplot2 theme
#' @export
#' @return ggplot2 theme
mytheme_pub = function(){
  theme_few() + 
  theme(
    axis.ticks = element_line(color = "black"), 
    panel.border =element_rect(color = "black", size = 1.2)
  )
}
