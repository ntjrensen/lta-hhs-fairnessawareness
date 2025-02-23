set.seed(9723)

## Laad de survey package

suppressMessages(library(survey))
library(gtsummary)
library(gt)
library(webshot2)

source("_Setup.R")

## Maak een samenvatting van de data
dfSummary <- dfOpleiding_inschrijvingen |>
  
  ## Verwijder kolommen die niet relevant zijn voor de analyse 
  select(-c(ID, Collegejaar)) |>
  
  ## Pas de labels van Uitval aan van True naar Ja, en van False naar Nee 
  mutate(Uitval = fct_recode(Uitval, 
                             "Nee" = "FALSE", 
                             "Ja" = "TRUE")) |>
  
  ## Pas de volgorde van de labels van Uitval aan 
  mutate(Uitval = fct_relevel(Uitval, "Ja", "Nee")) |>
  
  ## Maak een kolom met willekeurige gewichten tussen 10 en 20 
  mutate(Gewicht = runif(n(), 10, 20))
  
  ## Maak een survey design met de gewichten
  dfSummary_srvy <- survey::svydesign(ids = ~ 1,
                                      data = dfSummary,
                                      weights = ~ Gewicht)

## Toon deze als een samenvattende tabel
tblSummary_srvy <- dfSummary_srvy |>
  
  tbl_svysummary(
    by = Uitval,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no",
    percent = "row"
  ) |>
  
  ## Richt de vormgeving van de table in 
  modify_header(all_stat_cols() ~ "**{level}**, N={Number_to_readable(n)} ({style_percent(p)}%)") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Uitval**") |> 
  modify_header(label = "**Variabele**") |> 
  bold_labels() |> 
  modify_caption("**Studentkenmerken versus Uitval**") |> 
  add_p() |> 
  add_significance_stars( hide_p = FALSE, pattern = "{p.value}{stars}" ) |> 
  add_overall(last = TRUE, col_label = "**Totaal**, N = {Number_to_readable(N)}")
  
  ## Toon de tabel
  
  tblSummary_srvy

## Bewaar als afbeelding

gt_tbl <- as_gt(tblSummary_srvy) 
gt_tbl |> 
  gtsave("90_Test/cbs/tblSummary_srvy.png", expand = 10)

## Maak een samenvattende tabel voor 2 factoren
tblSummary_srvy_2f <- dfSummary_srvy |>
  
  tbl_strata(
    strata = Geslacht,
    .header = "**{strata}**, N = {n}",
    ~ .x |> tbl_svysummary(
      by = Uitval,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1,
      missing = "no",
      percent = "row"
    ) |>

    ## Richt de vormgeving van de table in
    modify_header(
      all_stat_cols() ~ "**{level}**, N={Number_to_readable(n)} ({style_percent(p)}%)"
    ) |>
      modify_header(label = "**Variabele**") |>
      bold_labels() |>
      modify_caption("**Studentkenmerken versus Geslacht en Uitval**") |>
      add_p() |>
      add_significance_stars(hide_p = FALSE, pattern = "{p.value}{stars}") |>
      add_overall(last = TRUE, col_label = "**Totaal**, N = {Number_to_readable(N)}")
    ```
    
  ) |>
  
  modify_spanning_header(all_stat_cols() ~ "**Uitval**")

## Toon de tabel

tblSummary_srvy_2f

## Bewaar als afbeelding

gt_tbl <- as_gt(tblSummary_srvy_2f)

gt_tbl |> gtsave("90_Test/cbs/tblSummary_srvy_2f.png", expand = 10)
