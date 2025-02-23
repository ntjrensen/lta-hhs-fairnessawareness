## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Test_ggcorpot.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Doel
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: Datasets
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 16-06-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(GGally)

# Zet de correlatiematrix om in een data frame geschikt voor ggplot
cor_data <- as.data.frame(as.table(dfCorplot))

ggcorplot <- ggplot(cor_data, aes(Var1, Var2, fill = Freq, label = round(Freq, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

# Toon de plot
print(ggcorplot)

ggcorplot <- dfCorplot |> 
  as.data.frame() |> 
  ggpairs(
    title = "Correlatieplot van variabelen in de dataset",
    lower = list(continuous = wrap("points", alpha = 0.5, size = 0.5)),
    diag = list(continuous = wrap("barDiag", binwidth = 0.5))
  )

corplot <- corplot +      
  
  ## Themes
  theme_minimal() +
  Set_LTA_Theme() +
  
  # Bepaal de titel, ondertitel en caption
  labs(
    title = "Correlatieplot van variabelen in de dataset",
    subtitle = "Clustering van variabelen op basis van correlatie",
    caption = sCaption,
    x = NULL,
    y = NULL
  ) +
  
  ## Voeg LTA elementen toe
  Add_LTA_theme_elements()
