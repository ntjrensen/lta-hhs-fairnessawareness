# Fairness Forecast Model Learning Technology & Analytics (LTA)

By Theo Bakker, Lecturer in Learning Technology & Analytics, t.c.bakker@hhs.nl

*To the [version history of the template](NEWS.md).*

This template is used to create forecasting models for the Learning Technology & Analytics Research Group (LTA) at THUAS. The template is based on the corporate identity of THUAS and uses Quarto.

## Software

To use this template you will need the following software, packages and fonts:

- [Quarto](https://quarto.org/docs/get-started/) version 1.4.11 or higher
- XQuartz](https://www.xquartz.org/) (for Mac only)
- The libraries from `_Config.R` (are installed automatically):
    - `library(tidymodels)`
    - `library(vip)`
    - `library(forcats)`
    - `library(performance)`
    - `library(dlookr)`
    - `library(gtsummary)`
    - `library(cli)`
    - `library(glue)`
    - `library(probably)`
    - `library(discrim)`
    - `library(klaR)`
    - `library(betacal)`
    - `library(doParallel)`

## Structure of files and folders

The template is composed of the following **files**:

- The index of the report: `index.qmd` (henceforth called Index).
- `_quarto.yml` with the general variables
- `README.md` with this text
- `NEWS.md` with the version history

It contains the following **folders**:

- `_output` with the reports created during automatic rendering.
- `_book` with the HTML files
- `_log` with the log files
- `_extensions` with the extensions for the Quarto environment
- `bibliography` with the bibliography files
- `brand` with the brand files
- `docs` for the documentation files
- `R/scripts` with R files
- `R/scss` with the scss file
- `R/functions` with the helper files (functions)
- `R/images` with the images
- `renv` for the packages from the R environment

**NB Git ignore**

- The `.gitignore` file is set up so that folders containing specific output are not included in the repo (e.g. `_log` and `_test`.

## Beforehand

Before you can use this template you should run `_Setup.R` once by clicking on 'Source'. - You will be told which libraries you still need to install.

## Using this template

If you are using the template for the first time, go through the following steps after the above:

- **Important to know**:
    - Variable texts that you can/should replace are in the header of Index.qmd.

## Structure of individual files

### Index

The index is made up of a number of components:

- The **YAML header** with:
    - `title`, `subtitle`: the title and the subtitle. Parts of these you set through the `params` further in the yml file. Using `params` will automatically adjust the title and subtitle if we create the file via Render for multiple study programmes.
    - `output-file`: the name of the pdf being generated.
    - `ltatemplate`: the version number of the Template; the number corresponds to the news page
    - `params` with options for the study programme and report:
        - `version`: the version of the report
        - `uitval`: the type of success for which we are modeling
        - `opleidingsnaam`: the full name of the study programme
        - `opleiding`: the code of the study programme
        - `opleidingsvorm`: the type of education in full (full-time, part-time, dual)
        - `opleidingsvorm_afkorting`: the type of education abbreviated (VT, DT, DU)
        - `selectie`: whether the study programme has a selection (false or true) - on this basis the rank variables are enclosed or not.
    - Because `_quarto.yml` is included, it is not necessary to include anything else in the index
        - in `_quarto.yml`, among other things, the date of the file is automatically determined, as well as the settings for the HTML page.
- The **setup** with the packages and options via `_Setup.R`:
    - **_Setup.R should not be modified**.
    - Because the `setup` chunk from the `index.qmd` does not read well in embedded .qmd files, it has been replaced by a `_Setup.R` file that is embedded in each page.
    - This file is executed only once per session; if there is a new version while working on your project, restart your R environment or adjust the variable `bReset_Setup <- F` once to `T`.
    - In `_Setup.R` you will find:
        - libraries + preferences for certain functions in case of conflicts
        - basic settings: current language, date report, Tableau colors and R6
        - default texts under images
        - general options for rendering knitr
        - file paths
    - See the inline documentation for further explanation.

# Content.

- The forecast models are built with the [tidymodels](https://www.tidymodels.org/) package from the tidyverse. A package that is in full development with an active developer group.
- If you want to learn tidy modeling, follow the [tutorials](https://www.tidymodels.org/start/) first and then read the online book: [Tidy Modeling with R](https://www.tmwr.org/) .
- We create two models that score reasonably well (penalized linear regression and random forest), require comparatively little computing power, and whose variables and their contributions to the model are known (a *whitebox* model).
- The operation of the models is explained in the text. Each report can thus be read independently.
- The best model is selected automatically. Based on that model, we draw conclusions about study programme dynamics.

## Known issues

- None

## Wishes

- None
