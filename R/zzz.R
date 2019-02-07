.onLoad <- function(libname, pkgname) {

  if(!require("ggplot2")){
    install.packages("ggplot2")
    require("ggplot2")
  }

  # Dr. Wang's ggplot2 APA theme
  theme_update(
    text = element_text(family = "Times", face = "plain", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 12),
    title = element_text(family = "Times", face = "bold", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 14),
    axis.title = element_text(family = "Times", face = "plain", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 12),
    axis.text = element_text(family = "Times", face = "plain", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 12),
    axis.text.x = element_text(family = "Times", face = "plain", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 12),
    axis.text.y = element_text(family = "Times", face = "plain", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 12),
    legend.text = element_text(family = "Times", face = "plain", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal", # values placed horizontally inside legend
    legend.title = element_text(family = "Times", face = "plain", colour="black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, size = 12),
    plot.background = element_rect(colour="black"), #background behind the plot
    panel.background = element_rect(colour="white", fill="white"), #visual part of plot
    panel.grid.major = element_line(colour="black") #lines
  )

  # ColorBrewer RdYlGn & GnYlRd 5-color:

RdYlGn5 <- c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641")
assign("RdYlGn5", RdYlGn5, envir = .GlobalEnv)

GnYlRd5 <- c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")
assign("GnYlRd5", GnYlRd5, envir = .GlobalEnv)

  # Texas Tech Primary Color Palette for ggplot
#  TtuPrimaryPalette <- c("#CC0000", "#000000", "#FFFFFF") #Primary Red, Primary Black, Primary White
#  assign("TtuPrimaryPalette", TtuPrimaryPalette, envir = .GlobalEnv)

  # Texas Tech Color Palette for ggplot
#  TtuSecondaryPalette <- c("#7B0F0F", "#1B1B1B", "#333333", "#F8F8F8", "#EAE0C6", "#F0DDAC", "#899FB4", "#20303f", "#B0B37B", "#4d5b2a")
  #Secondary Red, Black, Gray, White, Primary & Secondary Tan, Primary & Secondary Blue, Primary & Secondary Green
#  assign("TtuSecondaryPalette", TtuSecondaryPalette, envir = .GlobalEnv)

  # Texas Tech Color Palettes for red/green
  # ttu.green.red <- c("#4d5b2a", "#7B0F0F") # TTU secondary green/secondary red
  # assign("ttu.green.red", ttu.green.red, envir = .GlobalEnv)

  # ttu.red.green <- c("#7B0F0F", "#4d5b2a") # TTU secondary red/secondary green
  # assign("ttu.red.green", ttu.red.green, envir = .GlobalEnv)

  # United Way Color Palette for ggplot
#  UwMainPalette <- c("#005191", "#539ED0", "#FFB351", "#f57814", "#FF443B") #Dark Blue, Light Blue, Gold, Orange, Red
#  assign("UwMainPalette", UwMainPalette, envir = .GlobalEnv)

  # Uw2Colors <- c("#539ED0", "#FFB351") # Light Blue & Gold
  # assign("Uw2Colors", Uw2Colors, envir = .GlobalEnv)
  #
  # Uw3Colors <- c("#539ED0", "#005191", "#FF443B") # Light Blue, Dark Blue, & Red
  # assign("Uw3Colors", Uw3Colors, envir = .GlobalEnv)

#  Uw4Colors <- c("#005191", "#539ED0", "#FFB351", "#FF443B") #Dark Blue, Light Blue, Gold, Red
#  assign("Uw4Colors", Uw4Colors, envir = .GlobalEnv)

  # Uw5Colors <- c("#005191", "#539ED0", "#FFB351", "#f57814", "#FF443B") #Dark Blue, Light Blue, Gold, Orange, Red
  # assign("Uw5Colors", Uw5Colors, envir = .GlobalEnv)


}
