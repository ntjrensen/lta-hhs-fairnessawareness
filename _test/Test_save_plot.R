# Stel het aangepaste thema in
# Set_LTA_Theme()

## Sluit het _Setup.R bestand in
#source("_Setup.R")

#library(ggplot2)

## Bepaal de hoogte en breedte van afbeeldingen
nPlotWidth  <- 640
nPlotHeight <- 550

# Maak de plot met aangepaste titels en labels
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  ggtitle("Voorbeeldgrafiek") +
  theme(
    text = element_text(family = "sans")
  )

print(p)

# Bereken de afmetingen in inches (de standaard resolutie voor ggplot2 is 72 DPI)
plot_width_in <- nPlotWidth / 72
plot_height_in <- nPlotHeight / 72

ggplot2::ggsave(
  filename = "output.png",
  plot = p,
  width = plot_width_in,
  height = plot_height_in,
  dpi = 300,  # gebruik dpi om de resolutie in te stellen
  bg = "white",
  device = "png",
  units = "in"  # specificeer de eenheden voor de afmetingen
)
