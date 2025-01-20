# Prognosemodel Template Learning Technology & Analytics (LTA)

Door Theo Bakker, lector Learning Technology & Analytics, t.c.bakker\@hhs.nl

*Naar de [versiegeschiedenis van het template](NEWS.md).*

Dit template wordt gebruikt om prognosemodellen te maken voor het lectoraat Learning Technology & Analytics (LTA) van De HHS. Het template is gebaseerd op de huisstijl van De HHS en maakt gebruikt Quarto.

## Software

Om dit template te gebruiken heb je de volgende software, packages en fonts nodig:

-   [Quarto](https://quarto.org/docs/get-started/) versie 1.4.11 of hoger
-   [XQuartz](https://www.xquartz.org/) (alleen voor Mac)
-   De libraries uit `_Config.R` (worden automatisch geïnstalleerd):
    -   `library(tidymodels)`
    -   `library(vip)`
    -   `library(forcats)`
    -   `library(performance)`
    -   `library(dlookr)`
    -   `library(gtsummary)`
    -   `library(cli)`
    -   `library(glue)`
    -   `library(probably)`
    -   `library(discrim)`
    -   `library(klaR)`
    -   `library(betacal)`
    -   `library(doParallel)`

## Opbouw bestanden en folders

Het template is opgebouwd uit de volgende **bestanden**:

-   De index van het rapport: `Index.qmd` (voortaan Index genoemd)
-   `_quarto.yml` met de algemene variabelen
-   `README.Rmd` met deze tekst
-   `NEWS.Rmd` met de versiegeschiedenis

Het bevat de volgende **folders**:

-   `R/scripts` met bestanden die worden ingesloten (afbeeldingen en scss files)
-   `_output` met de rapporten die bij een automatische rendering worden gemaakt.
-   `R/functions` met de standaard voorbereidingen, het ltabase package en extra functies
-   `02_Plots` met de afbeeldingen
-   `docs` voor de documentatiebestanden
-   `renv` voor de packages uit de R omgeving

**NB Git ignore**

-   Het `.gitignore` bestand is zo ingericht dat folders die specifieke output bevatten niet meegaan naar de repo (bijv. `_00. _log` en `_test`.

## Vooraf

Voordat je dit template kan gebruiken kan je het beste 1x `_Setup.R` runnen door op 'Source' te klikken. - Je krijgt nu bericht welke libraries je nog moet installeren.

## Gebruik van dit template

Als je het template voor de eerste keer gebruikt, doorloop dan na het bovenstaande de volgende stappen:

-   **Belangrijk om te weten**:
    -   Variabele teksten die je kan/moet vervangen staan in de header van Index.qmd.

## Opbouw individuele bestanden

### Index

De index is opgebouwd uit een aantal onderdelen:

-   De **YAML header** met:
    -   `title`, `subtitle`: de titel en de ondertitel. Delen hiervan stel je in via de `params` verder in het yml bestand. Door `params` te gebruiken worden de titel en ondertitel automatisch aangepast als we het bestand via Render maken voor meerdere opleidingen.
    -   `output-file`: de naam van de pdf die wordt gegenereerd.
    -   `ltatemplate`: het versienummer van het LTA-template; het nummer correspondeert met de nieuwspagina
    -   `params` met opties voor de opleiding en het rapport:
        -   `versie`: de versie van het rapport
        -   `uitval`: de soort uitval waarvoor we een model maken
        -   `opleidingsnaam`: de volledige naam van de opleiding
        -   `opleiding`: de code van de opleiding
        -   `opleidingsvorm`: de opleidingsvorm voluit (voltijd, deeltijd, duaal)
        -   `opleidingsvorm_afkorting`: de opleidingsvorm afgekort (VT, DT, DU)
        -   `selectie`: of de opleiding een selectie heeft (false of true) - op basis hiervan worden de rankvariabelen ingesloten of niet.
    -   Doordat `_quarto.yml` is opgenomen, hoeft er niet meer in de index te worden opgenomen
        -   in `_quarto.yml` worden onder meer de datum van het bestand automatisch bepaald, maar ook de instellingen voor de HTML pagina.
-   De **setup** met de packages en opties via `_Setup.R`:
    -   **\_Setup.R mag niet aangepast worden**.
    -   Omdat de `setup` chunk uit de `Index.qmd` niet goed gelezen wordt in ingesloten .qmd bestanden, is deze vervangen door een `_Setup.R` bestand dat wordt ingesloten in elke pagina.
    -   Dit bestand wordt per sessie slechts 1x uitgevoerd; als er een nieuwe versie is terwijl je werkt aan je project, herstart dan je R omgeving of pas de variabele `bReset_Setup <- F` eenmalig aan naar `T`.
    -   In `_Setup.R` vind je:
        -   libraries + voorkeuren voor bepaalde functies bij conflicten
        -   basisinstellingen: huidige taal, datum rapport, Tableau kleuren en R6
        -   standaard-teksten onder afbeeldingen
        -   algemene opties voor het renderen van knitr
        -   bestandspaden
    -   Zie de inline documentatie voor de verdere toelichting.

# Inhoud

-   Het prognosemodel is gebouwd met het [tidymodels](https://www.tidymodels.org/) package uit de tidyverse. Een package dat volop wordt doorontwikkeld met een actieve ontwikkelaarsgroep.
-   Als je tidymodels wilt aanleren, doe dan eerst de [tutorials](https://www.tidymodels.org/start/) en lees daarna het online boek: [Tidy Modeling with R](https://www.tmwr.org/) .
-   We maken twee modellen die redelijk goed scoren (penalized lineaire regressie en random forest), naar verhouding weinig rekenkracht vragen en waarvan de variabelen en hun bijdrage aan het model bekend zijn (een *whitebox* model).
-   De werking van de modellen wordt uitgelegd in de tekst. Elk rapport is daarmee zelfstandig te lezen en bevat.
-   Het beste model wordt automatisch geselecteerd. Op basis van dat model trekken we conclusies over de dynamiek in de opleiding.

## Bekende issues

-   Geen

## Wensen

-   Geen
