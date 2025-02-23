# Fairness Awareness: Tutorial
Theo Bakker, The Hague University of Applied Sciences
2025-02-23

<a name="top"></a>

This tutorial explains how to create a **fairness analysis of the
progression of students in educational institutions in The Netherlands**
using the CEDA Fairness Awareness template.

## Table of contents

- [Introduction](#-introduction)
- [Why we study fairness, awareness, and
  equity](#why-we-study-fairness-awareness-and-equity)
- [Structure of files and folders](#structure-of-files-and-folders)
- [Using this template](#using-this-template)
- [Construction of individual files](#construction-of-individual-files)
  - [1. Index](#index)
  - [2. \_Setup.R](#_setupr)
  - [3. Quarto YAML](#quarto-yaml)
- [Structure of the reports](#structure-of-the-reports)
- [Rendering your first reports](#rendering-your-first-reports)
  - [Rendering all
    pages](#rendering-all-pages-of-a-report-using-the-terminal)
  - [Rendering individual
    pages](#rendering-individual-pages-of-a-report-using-the-terminal)  
- [Changing a study programme](#changing-a-study-programme)
- [Customizing the template](#customizing-the-template)
  - [Adjust design, house style, educational institution
    data](#adjust-design-house-style-educational-institution-data)
  - [Texts](#texts)
- [Contact](#contact)

## 🎒 Introduction

In this tutorial you will learn the following:

- How the concepts of Fairness, Awareness, and Equity are connected.
- How we structured two possible fairness reports.
- How the project files and folders are structured.
- How to use the template.
- How to construct individual files.
- How to customize the template for your own educational institution.
- What the known issues and wishes are, and how to contact us if you
  have any quesionts or suggestions.

## 💡 Why we study fairness, awareness, and equity

The Fairness Awareness Template is based on the concepts of Fairness,
Awareness, and Equity. These concepts are essential for the development
of a fair educational system. If we don’t study the fairness of our
institutions, we can’t be aware of the consequences of our actions in
policy, education or student counseling. If we are not aware of the
consequences of our actions, we can’t transform unfair educational
systems.

We use machine learning (ML) models to understand if there is bias in
the progression of students in educational institutions. The fairness of
an ML model can be assessed by examining the quality of its predictions
for different groups of students expressed in fairness metrics. If the
differences in predictions are disproportionally large, this is a sign
of bias within the data, unfairness for some groups of students, and
inequity \[ref\].

To learn more about the concepts of Fairness, Awareness, and Equity,
read the inaugural speech [No Fairness without
Awareness](https://zenodo.org/records/14204674) by Theo Bakker \[ref\].
The advanced report explains these concepts as well.

## 📚 Structure of the fairness reports

Fairness analyses consist of three main components: 1) one or more
prediction models, 2) an analysis of factors, and 3) a fairness
analysis.

Within the template, we offer two versions:

- The **advanced-report version** – for data scientists – contains an
  additional section on the underlying prediction models, explains the
  underlying concepts, and shows all relevant R code. This version has
  to be run first.
- The **basic-report version** – for policymakers – contains the
  conclusions, the fairness analysis and the analysis of factors.

In the basic-report, we reversed the order in which we present the pages
within the reports, as we start with a summary of the conclusions and
end with the predictions. This way, the reader can immediately see the
results of the analysis, as we found out in testing different reports
with teachers and policymakers in daily practice.

- **Introduction** (chapter 1)
  - The introduction to the report, the study programme and the research
    question.
- **Equity** (chapter 2)
  - The fairness analysis of the progression of students in the study
    programme.
- **Factors** (chapter 3)
  - The analysis of factors that influence the progression of students
    in the study programme.
- **Models** (chapter 4) - *only in the advanced version*
  - The development of prediction models for the students’ progression
    in the study programme.
  - The forecasting model is built with the
    [tidymodels](https://www.tidymodels.org/) package from the
    tidyverse. A package that is in full development with an active
    developer group. If you are not familiar yet with tidy modelling,
    first follow the [tutorials](https://www.tidymodels.org/start/) or
    read the online book [Tidy Modeling with R](https://www.tmwr.org/) .
  - We create two models (*penalized linear regression* and *random
    forest*), which require comparatively little computing power and
    whose variables and their contributions to the model are known (a
    *whitebox* model).
  - The operation of the models is explained – in advanced-report mode –
    in the text. Thus, each report can be read independently.
  - The best model is selected automatically. Based on that model, we
    will assess the fairness of a study programme.

The reports conclude with two additional pages:

- **References**
  - The references used in the report.
- **Appendix A: abbreviations**
  - A list of abbreviations used in the report.

You can **preview** the two versions of the report from the `_examples`
folder.

**NB.** The order of the chapters in the advanced report is as follows:
Introduction, Models, Factors, Equity, References, Appendix. This order
is necessary for the first rendering to build the report elements
correctly while considering their interdependence.

## 📂 Structure of files and folders

The template is composed of the following **files** (alphabetically):

- The index of the report: `index.qmd` (henceforth called Index)
- `_quarto.yml` with the general settings
- `_quarto-advanced-report.yml` with settings for the advanced report
- `_quarto-basic-report.yml` with settings for the basic report
- `_Setup.R` with general settings for R
- `_Setup_config.R` with configurations specific to your educational
  institution
- `ch1-introduction.qmd` with the introduction (chapter 1)
- `ch2-equity.qmd` with the fairness analysis (chapter 2)
- `ch3-factors.qmd` with the analysis of factors (chapter 3)
- `ch4-models.qmd` with the construction of the prediction models
  (chapter 4)
- `ch5-references.qmd` with the references (chapter 5)
- `quarto-render-basic-report.R` with a script to create the basic
  report with parameters
- `quarto-render-advanced-report.R` with a script to create the advanced
  report with parameters
- `renv.lock` with the versions of the packages
- `x-a-appendix-abbreviations.qmd` with an explanation of the
  abbreviation used in the reports (appendix A)

It also contains the following **folders** (alphabetically):

- `_advanced-report` and `_basic-report` with the reports created during
  automatic rendering in two variants
- `_examples` with two examples of an `_advanced-report` and
  `_basic-report` report for your reference
- `_extensions` with Quarto extensions for pandoc
- `_freeze` with elements that are unchanged and reused while rendering
- `_output` with reports by faculty and study programmes. The model fit
  results are stored here to speed up the rendering process.
- `bibliograhy` with references
- `brand` with the corporate identity files (logos, colors, fonts,
  layout in scss, colors)
- `docs` for the documentation files (README, TUTORIAL, LICENSE, and
  NEWS)
- `R/data` with general data sets
- `R/functions` with .R files for different types of functions
- `R/images` with images from the reports
- `R/qmd` with .qmd files that are embedded (text snippets)
- `R/scripts` with .R files to be embedded:
  - `fitted-model.R`: a snippet for the fitted-model chunk
  - `lf-explainer.R`: a snippet for the lf-explainer chunk
  - `load-dfpersona.R`: a snippet for the load-dfpersona chunk
- `R/vars` with the data dictonary of the variables
- `renv` for the packages from the R environment

**Git ignore**

- The `.gitignore` file is set up so that folders containing less
  relevant output are not included in the repo (e.g., `_log`, `_test`
  and `_graveyard`).

## 🚀 Using this template

If you have not yet installed the necessary software, do so now:

- Open `_Setup.R` and run it once by clicking on ‘Source’.
- You will now be instructed which libraries you need to install if you
  have not done so yet.
- After the installation, start a new session by restarting R and
  continue.

## 🔨 Construction of individual files

Before you render your first customized report, familiarize yourself
with the construction of the individual files in the template:

### 1. Index

The index.qmd file must exist if you are using a book template in
Quarto, as this template does; index.qmd of this project consists of the
following components:

- The **YAML header** with:
  - `subtitle`: the subtitle. You set part of this further in the yml
    file through the `params`. By using `params` the subtitle is
    automatically adjusted when you render the file.
  - `params` with options for the study programme and report:
    - `versie`: the version of the report
    - `succes`: the type of success for which we make a fairness
      analysis (e.g. Retention after 1 year, Retention after 2 years,
      etc.)
    - `model`: the name of the model. The model name can have the same
      name as the success parameter.
    - `propedeusediploma`: whether to take a propedeutic degree into
      account or not (false or true)  
    - `use_synthetic_data`: whether to use synthetic data (false or
      true).
    - `recreateplots`: whether you want to recreate the plots (false or
      true)
    - `opleiding`: the code of the study programme
    - `opleidingsvorm_afkorting`: the type of education abbreviated (VT,
      DT, DU)
    - `instroomselectie`: whether the study programme has entry
      selection (false or true). Based on this, variable `Rank` is
      enclosed or not.
  - `includes` with options to show or hide parts of pages
    - `introduction`: show or hide the introduction
    - `model_lr`: build the linear regression model or not
    - `model_rf`: build the random forest model or not
    - `model_svm`: build the support vector machine model or not
    - `final_fit`: show or hide the final fit
    - `conclusions`: show or hide the conclusions
    - `contact`: show or hide the contact data
    - `justification`: show or hide justification
    - `copyright`: show or hide copyright information
  - Because `_quarto.yml` is always output, there is no need to include
    more in the index
    - Among other things, `_quarto.yml` automatically determines the
      files’ date and the HTML page’s settings.
- The **setup** with the packages and options via `_Setup.R`: see
  explanation below.
- The **training information** via `R/qmd/header-studyprogram.qmd`
- The **summary of the probability equality analysis** via
  `R/qmd/equity-conclusions.qmd`
- The **contact** information via `R/qmd/footer-contact.qmd` - this page
  can be shown or hidden
- The **copyright** information via `R/qmd/footer-copyright.qmd` - this
  page can be shown or hidden

### 2. \_Setup.R

- **Do not modify \_Setup.R**.
- Because the `setup` chunk from the `index.qmd` does not read well in
  embedded .qmd files, it has been replaced by a `_Setup.R` file
  embedded in each page.
- This file is executed only once per session; if there is a new version
  while working on your project, restart your R environment for a new
  session or adjust the `bReset_Setup` variable at the top of the page
  from `bReset_Setup <- F` once to `T`.
- In `_Setup.R`, you will find:
  - A check on the environment: development or production
  - Settings for packages and functions:
    - libraries + preferences for specific functions in case of
      conflicts
    - the loading of brand settings
    - loading of fonts
    - loading of additional functions
    - loading of colors
    - defining the theme for images
    - a preference for the tidymodels settings
    - general options for rendering knitr
  - Configuration for:
    - file paths
    - debug options
    - gt-summary settings
    - training information
    - loading the data
    - plot settings
  - Content settings:
    - the long name of the study programme or faculty
    - the variables and their levels
    - the sensitive variables (on which fairness is examined)
    - the output paths for the data and the models
    - the settings for the models
    - the data for training, last fits and results.
- See the inline documentation for further explanation.

### 3. \_Setup_config.R

This file determines part of the configuration of the template.

- default parameters for the reports if \$params does not exist (see
  below)
- research settings that are used to build paths and captions
- the location of the data

### 4. Quarto YAML

As mentioned, the template works with `basic-report` and
`advanced-report` profiles. The basic-report profile contains the
conclusions, fairness analysis and factors analysis. The advanced-report
analysis also contains a section on the underlying prediction models,
explains the underlying concepts, and shows all relevant R code. Three
files accomplish this: `_quarto.yml`, `_quarto-basic-report.yml` and
`_quarto-advanced-report.yml`. The \_quarto.yml file contains both
reports’ general settings; the other files contain the specific settings
for the basic and advanced reports.

The **quarto.yml** file has the following options:

- The project settings (book) and associated details: author, date
  (based on the latest modification date), navigation
- The two profiles basic and advanced. If no profile is specified, the
  first profile prevails.
- The branding settings: the location of the house-style files
- The editor settings: work with the source code
- The execution settings: do not save markdown files and hide console
  output
- The citation settings: the location of the bibliography and the
  Citation Style Language (APA)
- The language settings: the language of the report
- The lightbox settings: enlarges images when you click on them
- The HTML settings: numbering, scrolling, the engine for mathematical
  texts, the font size (110%), the possibility for comments (via
  hypothesis), the theme, the output settings, the table of contents and
  links under ‘More information’.
- Other settings: configuration

The **quarto-basic-report.yml** file has the following options:

- The project name and output directory.
- The specific theme settings: the basic-report scss file.
- The specific execution settings: save files that have not changed
  (freeze); this setting ensures that when rendering, a \_freeze folder
  is created with elements that do not change and are reused in this
  template. If your web pages still don’t change after modifications,
  manually delete the \_freeze folder and re-render the template.
- The book settings: the title, chapter order and appendices. NB. This
  template hides Chapter 4: The development of prediction models.

The **quarto-advanced-report.yml** file has the following options:

- The project name and output directory.
- The specific theme settings: settings for showing R code and the
  advanced-report scss file.
- The specific execution settings: show the outcomes of the R-chunks
  (echo: true). This template does not use the freeze option to avoid
  reusing too much material.
- The book settings: the title, the order of the chapters and
  appendices. This template shows Chapter 4: the development of
  prediction models.

If you want to learn more about using Quarto profiles, visit the [Quarto
documentation](https://quarto.org/docs/projects/profiles.html).

## 📝️ Rendering your first reports

Now that you know the structure and contents of the project, you can
build your first report if you have not done so yet. Otherwise, try to
run a single page following the instructions below or continue to the
next step: Changing a study programme.

Since the template has several dependencies, you must render the pages
using the terminal. Do *not* render the pages using the Render button
within RStudio.

### Rendering all pages of a report using the terminal

To render a template in the terminal, follow these steps:

1.  Open a terminal in RStudio (second tab at the bottom of your
    screen).
2.  Run the following command for the *advanced-report* report:

``` r
quarto render --profile advanced-report
```

3.  Run the following command for the *basic-report* report:

``` r
quarto render --profile basic-report
```

### Rendering individual pages of a report using the terminal

To run an individual file (for example ch2-equity.qmd), follow these
steps:

1.  Open a terminal in RStudio (second tab at the bottom of your
    screen).
2.  Run the following command for the *advanced-report* report:

``` r
quarto render ch2-equity.qmd --profile advanced-report
```

3.  Run the following command for the *basic-report* report:

``` r
quarto render ch2-equity.qmd --profile basic-report
```

## ➿ Changing a study programme

### Rendering all pages of a report using a script with parameters

To change a study programme, don’t change the parameters in the yml
headers on every page (!), but use `quarto-render-basic-report.R` and
`quarto-render-advanced-report.R` to pass the adjusted parameters to the
template instead. These files use the `quarto_render` function to create
a report.

To render a template with these scripts, follow these steps:

1.  Open `quarto-render-advanced-report` for the *advanced-report*
    report and source it.
2.  Open `quarto-render-basic-report` for the *basic-report* report and
    source it. This script will remove the `_freeze` folder. Otherwise,
    the rendering would use existing pages from earlier settings.
3.  Change the settings in `lExecute_params` to change a programme. The
    preliminary options are presented after each setting.

## ✂️ Customizing the template

Now you have exercised how to render reports from the terminal or using
scripts, you can start customizing it. The template is initially set up
for The Hague University of Applied Sciences (THUAS), but you can adjust
the template to your own institution.

### Adjust design, house style, educational institution data

The design, house style and name of the educational institution in this
template are based on THUAS. You can adjust this to your insights. Since
Quarto 1.6, a \_brand.yml file has been available for this purpose.
However, the possibilities of working with brands are still limited. To
use the variables from \_brand.yml also in R, the file is also read into
\_Setup.R.

- The colors, fonts and logos are defined in `brand/_brand.yml` and
  `brand/scss/default.scss`:
  - In `_brand.yml` at `meta`, adjust the short and long name of the
    educational institution and the link to the website.
  - In `_brand.yml` at `color`, adjust the colors:
    institution-color-one, institution-color-two and
    institution-color-three.
  - In `_brand.yml` at `fonts`, adjust the fonts, if necessary. If
    necessary, also add the fonts in \_Setup.R (1.6). The new fonts are
    automatically installed the first time the template renders with
    them; this is repeated for every single page.
  - In `scss/default.scss`, modify the \$logo path. Add the logo file in
    the brand/logos folder, since the \_brand.yml file can’t include the
    path to the logo.
  - In `brand/colors/colors.R` adjust colors used in images if
    necessary.

### Texts

Texts about the author, educational institution or researchers are
situated in several additional locations.

- In `_quarto.yml`, adjust the name of the author of the report:
  `book:author`.
- In `_quarto.yml`, adjust the name of the educational institution:
  `brand:meta:name`.
- In `_Setup_config.R`: modify the metadata to determine the source and
  analysis for each image in the caption (footer of the image):
  - `lResearch_settings[[“sResearch_path”]]`: the name of this project
    to be used in filepaths
  - `lResearch_settings[[“sSucces_label”]]`: the name of measure for
    success
  - `lResearch_settings[[“sInstelling”]]`: the name of your institution
  - `lResearch_settings[[“sBron”]]`: who provided the dataset you are
    using
  - `lResearch_settings[[“sAnalyse”]]`: who performed the analysis

## 📫 Contact

If you have any questions or suggestions, please get in touch with the
author of this template: [Theo Bakker](mailto:t.c.bakker@hhs.nl)

[Back to top](#top)
