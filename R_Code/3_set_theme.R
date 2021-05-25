source(here("R_Code", "2_recodes.R"))
#### Load wlucolors package #### 
#remotes::install_github('sjkiss/wlucolors', force=T)
library(wlucolors)

#This sets the overall theme for all plots
theme_set(theme_minimal())
# This is where we can provide some modifications to the theme_minimal()
# WE can draw on these when we need them.
#Theme_remove_axis is theme_minimal but with the x-axis removed 
#It can be sued 
theme_1<-theme_minimal() + 
  theme(
    # axis.text.x = element_blank(),
    # axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.title=element_blank(),
    #legend.position="none", 
    plot.title=element_blank()
  )


