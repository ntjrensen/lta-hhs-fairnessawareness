# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# load.ltabase.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: No
#
# Purpose: Testing the current installation of ltabase.
#
# Dependencies: ltabase package
#
# Datasets: DNA
#
# Remarks:
# 1) None.
# 2) ___
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TODO:
# 1) ___.
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Version history:
# 21-02-2024: TB: File creation
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. Features for this page ####

# Function to determine the root folder of the project
Get_Rootdirectory <- function() {

    # Determine the root folder of the project
    sRootdirectory <- here::here()

    # Find the end position of the path to "LTA-HHS (Projectfolder)"
    nEindpositie <- regexpr("LTA-HHS \\(Projectfolder\\)", sRootdirectory)

    # Keep the substring until the end of the sequence found
    sRootdirectory <- substr(sRootdirectory, 1, nEindpositie + attr(nEindpositie, "match.length") - 1)

    return(sRootdirectory)
}

# Function to determine the path to the ltabase package
Get_Ltabase_path <- function() {

    sLtabase_directory <- file.path(Get_Rootdirectory(),
                                    "00 LTA Git/Git HHs/LTA_Packages/ltabase_releases")

    # Retrieve the latest version of ltabase from the directory and sort by name descending
    file_list <- list.files(sLtabase_directory, pattern = "*.tgz", full.names = TRUE) |>
        file.info() |>
        # Get the latest package based on ctime
        subset(ctime == max(ctime)) |>
        rownames() |>
        sort(decreasing = TRUE)

    # Store the latest version of ltabase in sLtabase_path
    sLtabase_path <- file_list[1]

    return(sLtabase_path)

}

# Function to determine the version of ltabase
Get_Ltabase_version <- function() {

    # Determine the version of ltabase based on sLtabase_path
    sLtabase_version <- gsub(".*_(.*)\\.tgz", "\\1", basename(Get_Ltabase_path()))

    return(sLtabase_version)
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. INSTALL AND LOAD THE LATEST VERSION OF LTABASE PACKAGE ####

# Verify that ltabase is installed:
# 1. If not install the latest version.
# 2. If yes, check that the latest version is installed
# 3. If not install the latest version

if (!requireNamespace("ltabase", quietly = TRUE)) {

    # Install ltabase
    install.packages(Get_Ltabase_path(), repos = NULL, type = 'source')

    cli::cli_alert("The package ltabase is installed to version {packageVersion('ltabase')}")

} else {

    # If the version of ltabase is not the latest version, install the latest version
    if (packageVersion("ltabase") < Get_Ltabase_version()) {

        # Installeer ltabase
        install.packages(Get_Ltabase_path(), repos = NULL, type = 'source')

        cli::cli_alert("The package ltabase has been updated to the latest version: {packageVersion('ltabase')}")

    } else {
        cli::cli_alert("The package ltabase is already installed in the latest version: {packageVersion('ltabase')}")
    }
}

# Load ltabase
library(ltabase)

# Ignore ltabase for renv
renv::settings$ignored.packages("ltabase")

