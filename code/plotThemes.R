# 
# code: Plot themes for sea ice visualizations
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: October 2021
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE PLOT THEMES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

seaice.plot.theme <- theme(plot.title = element_text(size = 16,
                                                     colour = "#303030",
                                                     face = "bold"),
                           plot.subtitle = element_text(size = 13,
                                                        colour = "#303030", 
                                                        face = "italic"),
                           axis.ticks.x = element_line(colour = "#C0C0C0"),
                           axis.ticks.y = element_blank(),
                           panel.background = element_rect(fill = "white",
                                                           colour = "#909090"),
                           panel.border = element_rect(fill = NA,
                                                       size = 0.25,
                                                       colour = "#C0C0C0"),
                           panel.grid.major.y = element_line(colour = "#C0C0C0",
                                                             size = 0.5,
                                                             linetype = 3),
                           panel.grid.major.x = element_blank(),
                           plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
                           axis.title = element_text(size = 13,
                                                     angle = 0,
                                                     face = "bold",
                                                     colour = "#303030"),
                           axis.text = element_text(size = 13,
                                                    angle = 0,
                                                    colour = "#303030",
                                                    lineheight = 0.7))

color.fill.3cats <- c("#332288", "#88CCEE", "#117733")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: DEFINE LEGEND GUIDES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

seaice.legend.guide <- 
  guides(colour = guide_legend(title.hjust = 1,
                               title.theme = element_blank(),
                               label.vjust = 0.5,
                               label.theme = element_text(size = rel(9),
                                                          angle = 0,
                                                          colour = "#505050",
                                                          lineheight = 0.75),
                               direction = "horizontal",
                               ncol = 1,
                               title.position = "left",
                               label.position = "right",
                               keywidth = unit(0.75, "cm"),
                               keyheight = unit(0.5, "cm")))
