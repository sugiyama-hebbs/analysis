# This formats ggplot
format_gg <- function(gplot, xlabel = NA, ylabel = NA, ptitle = NA,
                      pcol = NA, col_name = NA, col_lab = NA, fcol = NA, fill_name = NA, fill_lab = NA,
                      default_fsize = F, fsize_axis_text = 12, fsize_axis_title = 12, fsize_title = 12,
                      xlimit = NA, ylimit = NA, cartesian = T, expand_coord = T,
                      xticks = NA, yticks = NA, bg = F, show.leg = T, pos.leg = NA){
  
  theme_update(plot.title = element_text(hjust = .5)) # Set default alignment of plot title to be centered
  
  ## Labeling
  if (!is.na(xlabel))
    gplot <- gplot +  xlab(xlabel)
  
  if (!is.na(ylabel))
    gplot <- gplot + ylab(ylabel)
  
  if (!is.na(ptitle))
    gplot <- gplot + ggtitle(ptitle)
  
  ## Coloring
  # Color
  if (!is.na(pcol[1])){
    if (!is.na(col_name[1])&!is.na(col_lab[1])){
      gplot <- gplot + scale_color_manual(values = pcol, name = col_name, label = col_lab)
    } else if (!is.na(col_name[1])){
      gplot <- gplot + scale_color_manual(values = pcol, name = col_name)
    } else if (!is.na(col_lab[1])){
      gplot <- gplot + scale_color_manual(values = pcol, label = col_lab)
    } else {
      gplot <- gplot + scale_color_manual(values = pcol)
    }
  }  
  
  # Fill
  if (!is.na(fcol[1])){
    if (!is.na(fill_name[1])&!is.na(fill_lab[1])){
      gplot <- gplot + scale_fill_manual(values = fcol, name = fill_name, label = fill_lab)
    } else if (!is.na(fill_name[1])){
      gplot <- gplot + scale_fill_manual(values = fcol, name = fill_name)
    } else if (!is.na(fill_lab[1])){
      gplot <- gplot + scale_fill_manual(values = fcol, label = fill_lab)
    } else {
      gplot <- gplot + scale_fill_manual(values = fcol)
    }
  }  
  
  ## Back ground
  if (!bg){
    gplot <- gplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
  }
  
  ## Fontsize
  if (! default_fsize){
    gplot <- gplot + theme(
      title = element_text(size = fsize_title),
      axis.title = element_text(size = fsize_axis_title),
      axis.text = element_text(size = fsize_axis_text)
    )
  }
  
  
  ## Coordinate 
  if (cartesian){
    
    if (!is.na(xlimit[1]) & !is.na(ylimit[1])){
      gplot <- gplot + coord_cartesian(xlim = xlimit, ylim = ylimit, expand = expand_coord)
    } else if (!is.na(xlimit[1])){
      gplot <- gplot + coord_cartesian(xlim = xlimit, expand = expand_coord)
    } else if (!is.na(ylimit[1])){
      gplot <- gplot + coord_cartesian(ylim = ylimit, expand = expand_coord)
    } else {
      gplot <- gplot + coord_cartesian(expand = expand_coord)
    }
  } else {
    
    if (!is.na(xlimit[1]))
      gplot <- gplot + xlim(xlimit)
    
    if (!is.na(xlimit[1]))
      gplot <- gplot + ylim(ylimit)
  }
  
  if (!is.na(xticks[1]))
    gplot <- gplot + scale_x_continuous(breaks = xticks)
  
  if (!is.na(yticks[1]))
    gplot <- gplot + scale_y_continuous(breaks = yticks)
  
  ## legend
  if (!show.leg)
    gplot <- gplot + theme(legend.position = "none")
  
  if(!is.na(pos.leg)){
    
    if(pos.leg == "bl"){
      gplot <- gplot + theme(legend.justification = c("left","bottom"), legend.position = c(0,0), legend.background = element_rect(fill = NA))
    } else if (pos.leg == "br"){
      gplot <- gplot + theme(legend.justification = c("right","bottom"), legend.position = c(1,0),legend.background = element_rect(fill = NA))
    } else if(pos.leg == "tl"){
      gplot <- gplot + theme(legend.justification = c("left","top"), legend.position = c(0,1),legend.background = element_rect(fill = NA))
    } else if(pos.leg == "tr"){
      gplot <- gplot + theme(legend.justification = c("right","top"), legend.position = c(1,1),legend.background = element_rect(fill = NA))
    }
    
    
  }
  
  
  
  
  
  return(gplot)
  
}