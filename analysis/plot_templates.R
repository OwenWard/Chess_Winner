axis_title <- 16
title_size <- 18
axis_text_size <- 14
legend_text <- 14
line_size <- 1


theme_3col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(1, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 1.5),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 4, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}


theme_4col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(0.5, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 2),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 5, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}


theme_5col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(0.5, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 4),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 6.5, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}



theme_6col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(0.5, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 6),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 8, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}



theme_8col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(0.25, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 6),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 8, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}
