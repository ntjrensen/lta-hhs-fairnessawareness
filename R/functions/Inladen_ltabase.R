## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Installeren_ltabase.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Het toetsen van de huidige installatie van ltabase.
##
## Afhankelijkheden: ltabase package
##
## Datasets: Nvt
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 21-02-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0. Functies voor deze pagina ####

## Functie om de rootfolder van het project te bepalen
Get_Rootdirectory <- function() {

    # Bepaal de rootfolder van het project
    sRootdirectory <- here::here()

    # Zoek de eindpositie van het pad tot en met "LTA-HHS (Projectfolder)"
    nEindpositie <- regexpr("LTA-HHS \\(Projectfolder\\)", sRootdirectory)

    # Bewaar de substring tot het einde van de gevonden reeks
    sRootdirectory <- substr(sRootdirectory, 1, nEindpositie + attr(nEindpositie, "match.length") - 1)

    return(sRootdirectory)
}

## Functie om het pad naar het ltabase package te bepalen
Get_Ltabase_path <- function() {

    sLtabase_directory <- file.path(Get_Rootdirectory(),
                                    "00 LTA Git/Git HHs/LTA_Packages/ltabase_releases")

    ## Haal de laatste versie van ltabase op uit de directory en sorteer op naam descending
    file_list <- list.files(sLtabase_directory, pattern = "*.tgz", full.names = TRUE) |>
        file.info() |>
        ## Haal het meest recente package op basis van ctime
        subset(ctime == max(ctime)) |>
        rownames() |>
        sort(decreasing = TRUE)

    ## Bewaar de laatste versie van ltabase in sLtabase_path
    sLtabase_path <- file_list[1]

    return(sLtabase_path)

}

## Functie om de versie van ltabase te bepalen
Get_Ltabase_version <- function() {

    ## Bepaal de versie van ltabase op basis van sLtabase_path
    sLtabase_version <- gsub(".*_(.*)\\.tgz", "\\1", basename(Get_Ltabase_path()))

    return(sLtabase_version)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INSTALLEER EN LAAD DE LAATSTE VERSIE VAN LTABASE PACKAGE ####

## Controleer of ltabase is geïnstalleerd:
## 1. Zo niet installeer dan de laatste versie
## 2. Zo ja, controleer of de laatste versie is geïnstalleerd
## 3. Zo niet installeer dan de laatste versie

if (!requireNamespace("ltabase", quietly = TRUE)) {

    ## Installeer ltabase
    install.packages(Get_Ltabase_path(), repos = NULL, type = 'source')

    cli::cli_alert("Het package ltabase is geïnstalleerd naar versie {packageVersion('ltabase')}")

} else {

    ## Als de versie van ltabase niet de laatste versie is, installeer dan de laatste versie
    if (packageVersion("ltabase") < Get_Ltabase_version()) {

        ## Installeer ltabase
        install.packages(Get_Ltabase_path(), repos = NULL, type = 'source')

        cli::cli_alert("Het package ltabase is geüpdatet naar de laatste versie: {packageVersion('ltabase')}")

    } else {
        cli::cli_alert("Het package ltabase is al geïnstalleerd in de laatste versie: {packageVersion('ltabase')}")
    }
}

## Laad ltabase
library(ltabase)

## Ignore ltabase voor renv
renv::settings$ignored.packages("ltabase")

