# Fairness Analyse

Door Theo Bakker, lector Learning Technology & Analytics, t.c.bakker\@hhs.nl

*Naar de [versiegeschiedenis van het template](NEWS.md).*

Dit template wordt gebruikt om een fairness analyse te maken voor de Learning Technology & Analytics Research Group (LTA) van De Haagse Hogeschool. Het template is gebaseerd op de huisstijl van De Haagse Hogeschool en maakt gebruik van Quarto.

## Software

Om dit template te gebruiken heb je de volgende software, packages en fonts nodig:

-   [Quarto](https://quarto.org/docs/get-started/) versie 1.6.39 of hoger
-   [XQuartz](https://www.xquartz.org/) (alleen voor Mac)
-   De libraries uit `_Setup.R` (worden automatisch geïnstalleerd)

## Opbouw bestanden en folders

Het template is opgebouwd uit de volgende **bestanden**:

-   De index van het rapport: `Index.qmd` (voortaan Index genoemd)
-   `_quarto.yml` met de algemene instelligen
-   `_quarto-advanced.yml` met instellingen voor het advanced rapport
-   `_quarto-basic.yml` met instellingen voor het basic rapport
-   `_Setup.R` met algemene instellingen voor R
-   `ch1-introduction.qmd` met de introductie (hoofdstuk 1)
-   `ch2-equity.qmd` met de fairness analyse (hoofdstuk 2)
-   `ch3-factores.qmd` met de analyse van factoren (hoofdstuk 3)
-   `ch4-models.qmd` met de bouw van de voorspelmodellen (hoofdstuk 4)
-   `ch5-references.qmd` met de referenties (hoofdstuk 5)
-   `render-multiple.R` met een script om meerdere rapporten te maken
-   `render-single.R` met een script om 1 rapport te maken
-   `renv.lock` met de versies van de packages

Het bevat daarnaast de volgende **folders**:

-   `_advanced` en `_basic` met de rapporten die bij een automatische rendering worden gemaakt in twee varianten.
-   `_extensions` met Quarto extensions voor pandoc
-   `_freeze` met elementen die onveranderd zijn en worden hergebruikt
-   `_output` met rapporten per faculteit en opleiding
-   `bibiograhy` met referenties
-   `brand` met de huisstijlbestanden (logo's, kleuren, lettertypes, opmaak in scss, kleuren)
-   `docs` voor de documentatiebestanden (README en NEWS)
-   `R/data` met algemene datasets
-   `R/functions` met .R-bestanden voor verschillende soorten functies
-   `R/images` met afbeeldingen
-   `R/qmd` met .qmd-bestanden die worden ingesloten (tekst snippets)
-   `R/scripts` met .R-bestanden die worden ingesloten (TODO: UITWERKEN)
-   `R/vars` met de datadictonary van de variabelen
-   `renv` voor de packages uit de R omgeving

**NB Git ignore**

-   Het `.gitignore` bestand is zo ingericht dat folders die specifieke output bevatten niet meegaan naar de repo (bijv. `_log`, `_test` en `_graveyard`).

## Vooraf

-   Run voordat je dit template gebruikt 1x `_Setup.R` door op 'Source' te klikken. 
-   Je krijgt nu bericht welke libraries je nog moet installeren. 
-   Start nadat de installatie klaar is een nieuwe sessie.

## Gebruik van dit template

Als je het template voor de eerste keer gebruikt, doorloop dan na het runnen van `_Setup.R` de volgende stappen:

-   Configuratie
    -   Pas de instellingen in `_quarto.yml` aan naar de wensen van je project.
    -   Pas de instellingen in `_quarto-advanced.yml` en `_quarto-basic.yml` aan naar de wensen van je project.
    [Verder uitwerken]

## Opbouw van individuele bestanden

### Index

Het bestand index.qmd moet bestaan als je in Quarto een book template gebruikt; index.qmd is opgebouwd uit de volgende onderdelen:

-   De **YAML header** met:
    -   `subtitle`: de ondertitel. Een deel hiervan stel je in via de `params` verder in het yml bestand. Door `params` te gebruiken wordt de ondertitel automatisch aangepast als je het bestand rendert.
    -   `params` met opties voor de opleiding en het rapport:
        -   `versie`: de versie van het rapport
        -   `succes`: het soort succes waarvoor we een fairness analyse maken (bijv. Retentie na 1 jaar, Retentie na 2 jaar, etc.)
        -   `model`: de naam van het model. Dit kan dezelfe naam hebben als de succes parameter.
        -   `use_synthetic_data`: of je gebruik maakt van synthetische data (false of true)
        -   `recreateplots`: of je de plots opnieuw wilt maken (false of true)
        -   `faculteit`: de faculteit waarvoor het rapport wordt gemaakt
        -   `opleidingsnaam`: de volledige naam van de opleiding
        -   `opleiding`: de code van de opleiding
        -   `opleidingsvorm`: de opleidingsvorm voluit (voltijd, deeltijd, duaal)
        -   `opleidingsvorm_afkorting`: de opleidingsvorm afgekort (VT, DT, DU)
        -   `instroomselectie`: of de opleiding een instroomselectie heeft (false of true) - op basis hiervan wordt variabele `Rank` ingesloten of niet.
    -   `includes` met opties om onderdelen van pagina's wel of niet te tonen
        -   `inleiding`: toon wel of niet de inleiding
        -   `data`: ??
        -   `model_lr`: bouw wel of niet het lineaire regressiemodel
        -   `model_rf`: bouw wel of niet het random forest model
        -   `model_svm`: bouw wel of niet het support vector machine model
        -   `final_fit`: toon wel of niet de final fit
        -   `conclusions`: toon wel of niet de conclusies
        -   `contact`: toon wel of niet de contactgegevens
        -   `justification`: toon wel of niet de verantwoording
        -   `copyright`: toon wel of niet de copyright informatie
    -   Doordat `_quarto.yml` altijd wordt uitgevoerd, hoeft er niet meer in de index te worden opgenomen
        -   in `_quarto.yml` worden onder meer de datum van het bestand automatisch bepaald, maar ook de instellingen voor de HTML pagina.
-   De **setup** met de packages en opties via `_Setup.R`: zie de verder uitleg hieronder.
-   De **opleidingsinformatie** via R/qmd/header-studyprogram.qmd
-   De **samenvatting van de kansengelijkheidsanalyse** via R/qmd/equity-conclusions.qmd
-   De **contact** informatie via R/qmd/footer-contact.qmd - deze pagina kan wel of niet worden getoond
-   De **copyright** informatie via R/qmd/footer-copyright.qmd - deze pagina kan wel of niet worden getoond
    
### _Setup.R   

-   **\_Setup.R mag niet aangepast worden**.
-   Omdat de `setup` chunk uit de `index.qmd` niet goed gelezen wordt in ingesloten .qmd bestanden, is deze vervangen door een `_Setup.R` bestand dat wordt ingesloten in elke pagina.
-   Dit bestand wordt per sessie slechts 1x uitgevoerd; als er een nieuwe versie is terwijl je werkt aan je project, herstart dan je R omgeving voor een nieuwe sessie of pas de variabele `bReset_Setup <- F` eenmalig aan naar `T`.
-   In `_Setup.R` vind je:
    -   Een check op de omgeving: development of production
    -   Instellingen voor packages en functies:
        -   libraries + voorkeuren voor bepaalde functies bij conflicten
        -   het inladen van de brand instellingen
        -   het inladen van fonts
        -   het inladen van extra functies
        -   het inladen van kleuren
        -   het bepalen van het thema voor afbeeldingen
        -   een voorkeur voor de tidymodels instellingen
        -   algemene opties voor het renderen van knitr
    -   Configuratie voor:
        -   bestandspaden
        -   debug opties
        -   gt-summary instellingen
        -   default parameters voor de rapporten als $params niet bestaat
        -   opleidingsinformatie
        -   het inladen van de data
        -   overige instellingen: onderzoeksinstellingen, de caption bij afbeeldingen, plotinstellingen
    -   Content instellingen:
        -   de lange naam van de opleidings of faculteit
        -   de variabelen en hun levels
        -   de sensitieve variabelen (waarop fairness wordt onderzocht)
        -   de outputpaden voor de data en de modellen
        -   de instellingen voor de modellen
        -   de data voor training, last fits and resultaten
-   Zie de inline documentatie voor de verdere toelichting.
    
### Quarto YAML

Het template werkt met twee profielen: `basic` en `advanced`. Het basic profiel bevat de conclusies, de fairness analyse en de factoren analyse. De advanced analyse bevat ook een hoofdstuk over de achterliggende voorspelmodellen, legt de achterliggende concepten uit en toont alle relevante R-code. Om dit te realiseren wordt gebruik gemaakt van drie bestanden: `_quarto-basic.yml`, `_quarto-basic.yml` en `_quarto-advanced.yml`. Het _quarto.yml bestand bevat de algemene instellingen voor beide rapporten; de andere twee bestanden bevatten de specifieke instellingen voor de basic en advanced rapporten. 

Het **\_quarto.yml** bestand heeft de volgende opties:

-   De project instellingen (book) en bijbehorende details: auteur, datum (op basis van de laatste aanpassing), navigatie
-   De beide profielen: basic en advanced. Als geen profiel wordt opgegeven wordt het eerste profiel gebruikt.
-   De branding instellingen: de locatie van de huisstijlbestanden
-   De editor instellingen: werk met de brondcode
-   De executie instellingen: bewaar geen markdown files en toon niet de console output
-   De citatie instellingen: de locatie van de bibliografie en de Citation Style Language (apa)
-   De taal instellingen: de taal van het rapport
-   De lightbox instellingen: afbeeldingen worden uitvergroot als je erop klikt
-   De html instellingen: nummering, scrollen, de engine voor wiskundige teksten, de fontgrootte (110%), mogelijkheid voor commentaar (via hypothesis), het thema, de output instellingen, de inhoudsopgave en links onder 'Meer informatie'.
-   Overige instellingen: configuratie

Het **\_quarto-basic.yml** bestand heeft de volgende opties:

-   De naam van het project en de output-directory.
-   De specifieke thema instellingen: een eigen scss bestand.
-   De specifieke executie instellingen: bewaar bestanden die niet veranderd zijn (freeze); deze instelling zorgt ervoor dat bij het renderen een _freeze folder wordt aangemaakt met elementen die niet veranderen en worden hergebruikt in dit template. Als je merkt dat je webpagina's na aanpassingen toch niet veranderen, verwijder dan handmatig de _freeze folder en render het template opnieuw. 
-   De boek instellingen: de titel, de volgorde van de hoofdstukken en appendices. NB. Dit template verbergt hoofdstuk 4: de ontwikkeling van voorspelmodellen

Het **\_quarto-advanced.yml** bestand heeft de volgende opties:

-   De naam van het project en de output-directory.
-   De specifieke thema instellingen: instellingen voor het tonen van R-code en het eigen scss bestand.
-   De specifieke executie instellingen: toon de uitkomsten van de R-chunks (echo: true). Dit template maakt geen gebruik van de freeze optie om te voorkomen dat te veel materiaal wordt hergebruikt.
-   De boek instellingen: de titel, de volgorde van de hoofdstukken en appendices. NB. Dit template toont juist wel hoofdstuk 4: de ontwikkeling van voorspelmodellen.

Wil je meer leren over het gebruik van profielen? Bezoek dan de [Quarto documentatie](https://quarto.org/docs/projects/profiles.html).


# Inhoud

[TODO: uitleggen per hoofdstuk]
-   Het prognosemodel is gebouwd met het [tidymodels](https://www.tidymodels.org/) package uit de tidyverse. Een package dat volop wordt doorontwikkeld met een actieve ontwikkelaarsgroep.
-   Als je tidymodels wilt aanleren, volg dan eerst de [tutorials](https://www.tidymodels.org/start/) en lees daarna het online boek: [Tidy Modeling with R](https://www.tmwr.org/) .
-   We maken twee modellen (penalized lineaire regressie en random forest), die naar verhouding weinig rekenkracht vragen en waarvan de variabelen en hun bijdrage aan het model bekend zijn (een *whitebox* model).
-   De werking van de modellen wordt -- in advanced modus -- uitgelegd in de tekst. Elk rapport is daarmee zelfstandig te lezen.
-   Het beste model wordt automatisch geselecteerd. Op basis van dat model trekken we conclusies over de kansengelijkheidsdynamiek in een opleiding.

# Aanpassen van het template voor eigen gebruik

## Vormgeving, huisstijl, onderwijsinstellingsgegevens aanpassen

De vormgeving, huisstijl en naam van de onderwijsinstelling in dit template is gebaseerd op De Haagse Hogeschool. Dit kan je naar eigen inzichten aanpassen. Sinds Quarto 1.6 is hiervoor een _brand.yml bestand beschikbaar. De mogelijkheden hiervan zijn nog wel beperkt. Om de variabelen uit _brand.yml ook in R te kunnen gebruiken, wordt het bestand ook in _Setup.R ingelezen.

-   De kleuren, lettertypes en logo's zijn vastgelegd in brand/_brand.yml en brand/scss/default.scss:
    -   Pas in _brand.yml bij `meta` de korte en lange naam van de onderwijsinstelling aan en de link naar de website.
    -   Pas in _brand.yml bij `color` de kleuren aan: institution-color-one, institution-color-two en institution-color-three.
    -   Pas in _brand.yml bij `fonts` de lettertypes eventueel aan. Voeg in dat geval eventueel ook de lettertypes toe in _Setup.R. Bij de eerste keer dat het template rendert met de nieuwe fonts, worden deze automatisch geïnstalleerd.
    -   Pas in scss/default.scss het $logo-path aan. Voeg het instellingslogo toe in de brand/logos folder. Dit pad kan niet in de _brand.yml worden opgenomen.
    -   Pas kleuren die gebruikt worden in afbeeldingen eventueel aan in brand/colors/colors.R.

## Teksten

Teksten over de auteur, onderwijsinstelling of onderzoekers komen op een aantal extra locaties voor.

-   Pas in _quarto.yml de naam van de auteur van het rapport aan: book:author.
-   Pas in _quarto.yml de naam van de onderwijsinstelling aan: brand:meta:name.
-   Pas in _Setup.R: 2.7.1 de metadata aan: `lResearch_settings[["sInstelling"]]`, `lResearch_settings[["sBron"]]` (door wie de dataset die je gebruikt is geleverd) en `lResearch_settings[["sAnalyse"]]` (wie de analyse heeft uitgevoerd). Deze variabelen bepalen voor elke afbeelding de bron en de analyse in de caption (footer van de afbeelding).

# Bekende issues

-   Geen

# Wensen

-   Logo kunnen opnemen in _brand.yml om vervolgens in basic.scss te kunnen gebruiken.






