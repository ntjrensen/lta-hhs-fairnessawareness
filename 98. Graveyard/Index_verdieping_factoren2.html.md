---
title: "2. Factoranalyse - Uitval na 1 jaar  - Zonder P"
subtitle: "GVS | B Opleiding tot Verpleegkundige (HBO-V) - voltijd - versie 1.0"

## Auteur en datum
author: "Theo Bakker, lector Learning Technology & Analytics, De HHs"
date: last-modified

## LTA Template
ltatemplate: 0.9.1.9000

## Format en output
output-file: "lta-hhs-tidymodels-uitval-verdieping-factoren.html"

## Parameters        
params:
  versie: "1.0"
  uitval: "Uitval na 1 jaar"
  propedeusediploma: "Zonder P" ## Nvt/Met P/Zonder P
  faculteit: "GVS"
  
  ## Recreate plots
  recreateplots: true
  
  # GVS:HBO-V
  opleidingsnaam: "B Opleiding tot Verpleegkundige"
  opleiding: "HBO-V"
  opleidingsvorm: "voltijd"
  opleidingsvorm_afkorting: "VT"
  selectie: false
  
  ## GVS:MT
  # opleidingsnaam: "B Mens en Techniek"
  # opleiding: "MT"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: false
  
  ## GVS:HDT
  # opleidingsnaam: "B Huidtherapie"
  # opleiding: "HDT"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: true
  
  ## FDR:ORM DT
  # opleidingsnaam: "B Ondernemerschap Retail Management"
  # opleiding: "ORM"
  # opleidingsvorm: "deeltijd"
  # opleidingsvorm_afkorting: "DT"
  # selectie: false
  
  ## SWE:SW
  # opleidingsnaam: "B Social Work"
  # opleiding: "SW"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: true
  
  ## TIS:IPO
  # opleidingsnaam: "B Industrieel Product Ontwerpen"
  # opleiding: "IPO"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: false
  
  ## TIS:EC
  # opleidingsnaam: "B Elektrotechniek"
  # opleiding: "E"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: false
  
  ## TIS:B
  # opleidingsnaam: "B Bouwkunde"
  # opleiding: "B"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: false
  
  ## BFM:AC
  # opleidingsnaam: "B International Business"
  # opleiding: "IB-ES-3"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: false
  
  ## BFM:AC
  # opleidingsnaam: "B Accountancy"
  # opleiding: "AC"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: false
  
  ## ITD:CMD
  # opleidingsnaam: "B Communication and Multimedia Design"
  # opleiding: "CMD"
  # opleidingsvorm: "voltijd"
  # opleidingsvorm_afkorting: "VT"
  # selectie: false
  
  ## Author
  author: "Theo Bakker, lector Learning Technology & Analytics"
  
## Content
includes:
  inleiding:      true
  data:           true
  model_lr:       true
  model_rf:       true
  model_svm:      false
  final_fit:      true
  conclusies:     true
  verantwoording: true
  nextsteps:      true
  copyright:      true
---



<!-- Inleiding -->

::: {.content-hidden unless-meta="includes.inleiding"}
# Inleiding

Na de basis-analyse van de data en het bouwen van de prognosemodellen, gaan we in deze verdiepende analyse dieper in op de factoren van de modellen. Het doel is beter te begrijpen hoe de factoren precies de uitval verklaren. Deze verdiepende factoranalyse heeft een aantal stappen:

1.  We lezen de bewerkte dataset in en de modellen die we in de basis-analyse hebben gemaakt.
2.  We maken een *explainer* om de modellen beter te begrijpen en te kunnen uitleggen. Dit lichten we later in deze pagina toe.
3.  We gebruiken het beste model om de prognose te verklaren en te begrijpen. We kijken naar de bijdrage van de variabelen aan de voorspelling en passen het model toe op de meest voorkomende studenten.
4.  We kijken tot slot naar de stabiliteit van de invloed van de verklarende variabelen met behulp van *Shapley* waarden.
:::

<!-- Data -->

# Voorbereidingen

## Laad de data

We laden de bewerkte data en prognosemodellen in voor:

**Opleiding**: GVS \| B Opleiding tot Verpleegkundige (HBO-V), voltijd, eerstejaars - **Uitval na 1 jaar**

::: cell
``` {.r .cell-code}
## Bepaal de paden
sData_outputpath         <- Get_Model_outputpath(mode = "data")
sFittedmodels_outputpath <- Get_Model_outputpath(mode = "last-fits")
sModelresults_outputpath <- Get_Model_outputpath(mode = "modelresults")

## Laad de data voor de opleiding: data, last fits en model results
dfOpleiding_inschrijvingen <- rio::import(sData_outputpath, trust = TRUE)
lLast_fits                 <- rio::import(sFittedmodels_outputpath, trust = TRUE)
dfModel_results            <- rio::import(sModelresults_outputpath, trust = TRUE)

# Pas de Uitval variabele aan naar numeric (0/1), 
# zodat er een explainer van gemaakt kan worden
dfOpleiding_inschrijvingen$Uitval <- as.numeric(dfOpleiding_inschrijvingen$Uitval) - 1

## Laad de persona's
source("01. Includes/Studentpersonas_opleidingen.R")
dfPersona_all <- Get_dfPersona()
```
:::

# Verdiepende analyse van het model

We weten vanuit de basis-analyse welke variabelen van invloed zijn, maar niet hoe en in welke richting ze uitval verklaren: dragen ze sterk bij of juist niet, verhogen of verlagen ze uitval? Om het model beter te begrijpen en te kunnen uitleggen, maken met behulp van het [`Dalex` package](https://dalex.drwhy.ai) een *explainer*. Dit package is ontwikkeld om beter uit te leggen welke verklarende variabelen van belang zijn en wat deze voor een effect hebben in een model. Een explainer is een model-onafhankelijke *wrapper* om het model heen en geeft inzicht in de voorspellingen van het model en de bijdrage van de variabelen aan de prognose. Een explainer maakt het verder mogelijk om modellen onderling te vergelijken en benchmarken.

## Recap van de basis-analyse

Uit de basis-analyse kwam de volgende lijst met meest voorspellende variabelen:

## Maak een explainer

We gaan nu een stap verder met behulp van het `Dalex` package. Op basis van het tidymodels model extraheren we de informatie voor de explainer van Dalex.

::: cell
``` {.r .cell-code}
## Extraheer het fitted model en de workflow
fitted_model <- last_fit |>
  extract_fit_parsnip()

workflow <- last_fit |>
  extract_workflow()

# Maak een explainer
explain_lm <- DALEX::explain(
  model = workflow,
  data = dfOpleiding_inschrijvingen,
  y = dfOpleiding_inschrijvingen$Uitval,
  label = "Linear Regression")
```

::: {.cell-output .cell-output-stdout}
```         
Preparation of a new explainer is initiated
  -> model label       :  Linear Regression 
  -> data              :  1818  rows  27  cols 
  -> target variable   :  1818  values 
  -> predict function  :  yhat.workflow  will be used (  default  )
  -> predicted values  :  No value for predict function target column. (  default  )
  -> model_info        :  package tidymodels , ver. 1.2.0 , task classification (  default  ) 
  -> predicted values  :  numerical, min =  0.09233298 , mean =  0.3963696 , max =  0.738768  
  -> residual function :  difference between y and yhat (  default  )
  -> residuals         :  numerical, min =  -0.7024894 , mean =  0.003520435 , max =  0.8469217  
  A new explainer has been created!  
```
:::
:::

::: cell
``` {.r .cell-code}
## Bereken de model parts op basis van de RMSE
mp_rmse <- model_parts(explain_lm, loss_function = loss_root_mean_square)

p_mp_rmse <- plot(mp_rmse) +
  
  ## Themes
  theme_minimal() +
  Set_LTA_Theme() +
  
  ## Titel, ondertitel en caption
  labs(
    title = "Belangrijkste factoren",
    subtitle = "Root Mean Square Error (RMSE) na permutaties",
    caption = sCaption,
    x = NULL,
    y = NULL
  ) +
  
  # theme(
  #     legend.position = "none",
  #     plot.title = element_text(size = 14, face = "bold"),
  #     axis.text.y = element_text(size = 10)
  #   ) 

    ## Verberg de legenda
    theme(
      legend.position = "none"
    ) +
    
    ## Voeg LTA elementen toe
    Add_LTA_theme_elements()

## Print de plot
suppressWarnings(print(p_mp_rmse))
```

::: cell-output-display
![](Index_verdieping_factoren_files/figure-html/lf_model_parts-1.png){width="672"}
:::

``` {.r .cell-code}
## Bewaar de plot
sPlotPath <- file.path(Get_Plot_outputpath(plotname = "lf_model_parts_rmse"))

suppressWarnings(
  Finalize_Plot(
    plot_name = p_mp_rmse,
    save_filepath = sPlotPath
  ))
```
:::

De eerste analyse is de Root Mean Square Error (RMSE) van de voorspellingen van het model. Dit geeft een indicatie van de nauwkeurigheid van het model. Hoe lager de RMSE, hoe beter het model de uitval voorspelt. In de grafiek is de RMSE per verklarende variabele berekend \[*nader uitwerken*\].

## Inspecteer variabelen met de meeste invloed

Nu we deze explainer hebben, kunnen we het model toepassen op de meest voorkomende student. We kijken eerst naar de meest voorkomende student in het algemeen. We analyseren vervolgens de meest voorkomende student in meerdere groepen: naar vooropleiding, geslacht, leeftijd en aansluiting, etc. Om de meest voorkomende student te bepalen, gebruiken we de frequentste waarden van de verklarende variabelen in de dataset per groep.

*Ter illustratie* Stel dat we een onderscheid maken tussen mbo en havo studenten, dan bepalen we de mediaan van numerieke variabelen en de frequentste waarde van categorische variabelen. Van de leeftijd kan misschien 20 het vaakst voorkomen, etc. De meest voorkomende student is dus geen daadwerkelijke student, maar een representatie van de groep op basis van de frequentste kenmerken.

We kijken hiermee naar de voorspelling van het model per groep en de bijdrage van de verklarende variabelen aan die specifieke voorspelling. Dit geeft een verder inzicht in de werking van het model. Een categorie met 20 studenten of minder laten we buiten beschouwing.

### Toelichting op de opbouw van de kans op uitval

De opbouw van het model bestaat uit een *intercept*, gevolgd door verklarende variabelen die een verschil maken ten opzichte van die intercept. De intercept is de basiskans op uitval voor alle studenten. Deze kans is voor de B Opleiding tot Verpleegkundige (HBO-V) voltijd 39,6%. De cumulatieve bijdrage van de variabelen aan de voorspelling kan positief of negatief zijn. Een positieve bijdrage betekent dat de variabele de kans op uitval verhoogt, een negatieve bijdrage betekent dat het de kans op uitval verlaagt.

Het kan zijn dat nieuwe variabelen geen invloed meer hebben op de kans. Dit betekent niet per se dat ze niet belangrijk zijn. Het kan zijn dat de invloed die ze hebben op de kans al is 'afgevangen' door variabelen die eerder in het model zijn opgenomen. Een voorbeeld: de variabele `Cijfer_CE_VO_missing = Ja` betekent dat een student geen VO cijfers heeft voor het centraal schriftelijk examen. Dit geldt voor vrijwel alle MBO studenten. Doordat de variabele Cijfer_CE_VO_missing de kans op uitval net wat sterker beïnvloedt, komt `Vooropleiding = MBO` niet meer voor als invloedrijke variabele, maar is dit wel de achterliggende reden dat het cijfer ontbreekt.

Uiteindelijk tellen alle verklarende variabelen op tot een definitieve voorspelling die per persoon verschilt, afhankelijk van hun persoonlijke kenmerken per variabele.

### De meest voorkomende student

We kijken eerst naar de meest voorkomende student in de opleiding. We analyseren de kans op uitval voor deze *fictieve* student en de bijdrage van de variabelen aan die kans. Daarbij tonen we de verdeling van de voorspellingen voor deze student voor alle variabelen en per variabele. Dit laat zien welke variabelen belangrijk zijn, naar welke kant de verdeling neigt en welke spreiding de kansverdeling heeft. Wat betekent dit?

-   **All data** - De eerste variabele `all data` is eigenlijk geen variabele, maar geeft aan wat alle data samen aan kans op uitval voorspellen. Variabelen die daarna bovenaan staan, wegen het zwaarst in de voorspelling van de kans.
-   **Richting** - Als de verdeling van de kansen naar de rechterkant van de x-as gaat, draagt deze variabele meer bij aan een toename op de kans op uitval; als deze naar de linkerkant beweegt, draagt deze variabele juist bij aan een afname op de kans op uitval.
-   **Spreiding** Als de spreiding breed is, geeft dit aan dat er binnen deze variabele veel variatie is in de kans op uitval en er voorzichtig mee omgegaan moet worden. Als de spreiding heel smal is, betekent dit dat de variabele weinig of geen invloed heeft op de kans op uitval. Deze variabelen bevinden zich op de intercept.

![](Index_verdieping_factoren_files/figure-html/lf_break_down_distribution_all-1.png){width="672"}

Nu de algemene opbouw van de kans op uitval bekend is voor de meest voorkomende student, gaan we verder met een analyse van de meest voorkomende studenten *per groep*. \[dit aanvullen met de belangrijkste conclusies op basis van de intercept en verschillen in uiteindelijke kans\]

### Naar geslacht

::: {.panel-tabset}

## M

![](Index_verdieping_factoren_files/figure-html/lf_break_down-1.png)

## V

![](Index_verdieping_factoren_files/figure-html/lf_break_down-2.png){width="672"}

:::

### Naar vooropleiding

::: {.panel-tabset group = 'Vooropleiding'}

![](Index_verdieping_factoren_files/figure-html/lf_break_down-3.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-4.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-5.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-6.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-7.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-8.png){width="672"}

:::

### Naar aansluiting

Subtotaal voor Aansluiting: 2e Studie, Overig is te laag voor een betrouwbare analyse.

::: {.panel-tabset group = 'Aansluiting'}

![](Index_verdieping_factoren_files/figure-html/lf_break_down-9.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-10.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-11.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-12.png){width="672"}![](Index_verdieping_factoren_files/figure-html/lf_break_down-13.png){width="672"}

:::

## Shapley

Tot slot van deze factorentanalyse kijken we naar de stabiliteit van de invloed van de verklarende variabelen. We gebruiken hiervoor *Shapley* waarden. Anders dan bij de vorige modellen, houdt Shapley rekening met een andere volgorde van de variabelen. De volgorde van de variabelen is cumulatief (additief) en maakt dus uit voor de bijdrage aan het model: als er een andere variabele al in het model is toegevoegd, heeft dat invloed op de daaropvolgende variabele. Een Shapley analyse permuteert de volgorde van de variabelen om daarmee de verschillen te berekenen in de bijdrage aan de voorspelling. Zo krijgen we nog beter zicht op het belang en de invloed van de individuele variabelen in het voorspelmodel. Variabelen zonder bijdrage hebben we verwijderd.

::: cell
``` {.r .cell-code}
## Bewaar de plot
sPlotPath <- file.path(Get_Plot_outputpath(plotname = "lf_shapley"))

if(params$recreateplots == TRUE) {

  ## Bepaal de Shapley waarden
  lf_shapley <- 
    predict_parts(
      explainer = explain_lm,
      new_observation = dfPersona_all[1, ],
      type = "shap",
      B = 20
    )

  ## Zet deze om naar een dataframe
  dfShapley <- Get_dfShapley(lf_shapley)

  ## Bouw de plot
  shapley_plot <- Get_Shapley_plot(dfShapley)
  
  ## Print de plot
  suppressWarnings(print(shapley_plot))

  ## Sla de plot op
  suppressWarnings(
    Finalize_Plot(
      plot_name = shapley_plot,
      save_filepath = sPlotPath
    ))

} else {

  ## Print de bestaande plot
  knitr::include_graphics(sPlotPath)

}
```

::: cell-output-display
![](Index_verdieping_factoren_files/figure-html/lf_shapley-1.png){width="672"}
:::
:::

**Toelichting:**

-   De variabelen met blauwe balken *verlagen* de kans op uitval, de variabelen met rode balken *verhogen* de kans op uitval.
-   De boxplot in iedere balk geeft de spreiding van de bijdrage van de variabelen aan de voorspelling weer. Hoe breder de boxplot, des te meer variatie in de bijdrage van de variabele aan de voorspelling.
-   De positie van de variabele geeft het belang van de variabele aan in de voorspelling. Hoe hoger de variabele, des te belangrijker de variabele is in de voorspelling.

<!-- Conclusies -->

::: {.content-hidden unless-meta="includes.conclusies"}
# Conclusies

Hier komen de overige conclusies.
:::

<!-- Next steps -->

::: {.content-hidden unless-meta="includes.nextsteps"}
# Vervolgstappen: kansengelijkheid

De volgende stap (stap 3) is te onderzoeken of er binnen deze opleiding binnen deze modellen **kansengelijkheid** bestaat. Dit doen we door de accuraatheid van de modellen te evalueren voor verschillende groepen studenten. Als de mate van accuraatheid van de voorspellingen van het model voor verschillende groepen studenten sterk uiteenloopt kan er sprake zijn van een bias en duiden op kansenongelijkheid. Dit is het onderwerp van de volgende en laatste analyse.
:::

<!-- Verantwoording -->

::: {.content-hidden unless-meta="includes.verantwoording"}
 

**Verantwoording**

Deze analyse maakt deel uit van het onderzoek naar kansengelijkheid van het lectoraat Learning Technology & Analytics van De Haagse Hogeschool: [No Fairness without Awareness](https://www.dehaagsehogeschool.nl/onderzoek/kenniscentra/no-fairness-without-awareness) \| Het rapport is door het lectoraat ontwikkeld in [Quarto](https://quarto.org/) 1.4.549. \| Template versie: 0.9.1.9000
:::

<!-- Copyright -->

::: {.content-hidden unless-meta="includes.copyright"}
 

**Copyright**

Dr. Theo Bakker, Lectoraat Learning Technology & Analytics, De Haagse Hogeschool © 2023-2024 Alle rechten voorbehouden.
:::

<!-- Opschonen -->

::: cell
:::
