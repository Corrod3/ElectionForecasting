
# RMSE

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



### rmse function
rmse.func <- function(df) {
  for (j in unique(as.character(df$datum))) {
    for (i in unique(df$method)) {
      vec.f <- unlist(df[df$method == i & df$datum == j, which(names(df) %in% partynames)])
      vec.sz <- unlist(df[df$method == "sz.rolling.av" & df$datum == j, which(names(df) %in% partynames)])
      df$rmse[df$method == i & df$datum == j] <- rmse(vec.sz, vec.f)
    } }
  return(df)
}

### plot function
# Plot function
shares.plot <- function(share.frame){
  ggplot(data = share.frame, aes(x = vote_nextelection_de, y = shares)) +
    geom_bar(aes(fill = vote_nextelection_de), stat = "identity") + 
    scale_fill_manual(values = farben) +
    scale_colour_manual(values = farben) +
    theme_classic() +
    scale_x_discrete(limits = position, name = "Major parties") +
    ylab("Vote Share in %") +
    guides(fill = FALSE) +
    geom_text(aes(label=paste0(round(shares,2),"%"), y  =shares + 1.1), size = 3.5) 
  
  #ggsave(file = "./Grafiken/plot.png")
}
