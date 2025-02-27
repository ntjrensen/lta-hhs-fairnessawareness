# Fairness Awareness: Analysis of Equity in Education
Theo Bakker, The Hague University of Applied Sciences
2025-02-27

<a name="top"></a>

<!-- badges: start -->

<!-- badges: begin -->

<!-- [![](https://img.shields.io/github/last-commit/LTA-HHs/lta-hhs-tidymodels-studiesucces.svg)](https://github.com/LTA-HHs/lta-hhs-tidymodels-studiesucces/commits/main) -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: Attribution-ShareAlike 4.0
International](https://img.shields.io/badge/license-Attribution--ShareAlike%204.0%20International-blue.svg)](https://cran.r-project.org/web/licenses/Attribution-ShareAlike%204.0%20International)

<!-- badges: end -->

<img src="figures/fairness-awareness-hex.png" align="right" height="160"/>

This <a
href="https://community-data-ai.npuls.nl/groups/view/44d20066-53a8-48c2-b4e9-be348e05d273/project-center-for-educational-data-analytics-ceda"
target="_blank">Npuls CEDA</a> <sup>🔗</sup> project makes it possible
to create a fairness analysis to study equity (*kansengelijkheid*)
within progression of students in educational institutions in The
Netherlands (mbo, hbo, and wo).

## Table of contents

- [About](#-about)
- [Getting started](#-getting-started)
  - [Prerequisites](#prerequisites)
  - [Installing](#installing)
- [Rendering your first
  reports](#-rendering-your-first-fairness-reports)
  - [Render the `_advanced-report`
    project](#render-the-_advanced-report-using-the-default-settings)
  - [Render the `_basic-report`
    project](#render-the-_basic-report-using-the-default-settings)
  - [Render your own
    reports](#create-and-render-your-institutional-reports)
- [Deployment](#-deployment)
- [Learn more on fairness, awareness, and
  equity](#-learn-more-on-fairness-awareness-and-equity)
- [Built with](#-built-with)
- [Contributing](#-contributing) <!-- - [Versioning](#versioning) -->
- [Authors](#-authors)
- [License](#-license)
- [Acknowledgments](#-acknowledgments)
- [Known issues](#-known-issues)
- [Wishes](#-wished)
- [Contact](#-contact)

## 🎓 About

This project is a template for creating a **fairness analysis of
students’ progression in educational institutions in The Netherlands**.
The template is based on the
<a href="https://quarto.org/docs/get-started/"
target="_blank">Quarto</a> <sup>🔗</sup> framework and uses the
<a href="https://www.tidyverse.org/" target="_blank">Tidyverse</a>
<sup>🔗</sup> coding standard. The template is developed by the <a
href="https://www.dehaagsehogeschool.nl/onderzoek/lectoraten/learning-technology-analytics"
target="_blank">Learning Technology &amp; Analytics (LTA)</a>
<sup>🔗</sup> research group of The Hague University of Applied Sciences
for the <a
href="https://community-data-ai.npuls.nl/groups/view/44d20066-53a8-48c2-b4e9-be348e05d273/project-center-for-educational-data-analytics-ceda"
target="_blank">Npuls CEDA</a> <sup>🔗</sup> project project.

## 🚀 Getting started

These instructions will get you a copy of the project up and running on
your local machine for development and testing. See the
[deployment](#deployment) for notes on deploying the project on a live
system.

### Prerequisites

To start working with this project, make sure you have the following
software installed following the links provided:

**R, Rstudio, Quarto, XQuartz (for Mac only)**

- RStudio version 2024.12.0 or higher with R version 4.4.2 or higher -
  <a href="https://posit.co/download/rstudio-desktop/"
  target="_blank">Posit</a> <sup>🔗</sup>
- XQuartz 2.8.5 or higher (for Mac only) -
  <a href="https://www.xquartz.org/" target="_blank">XQuartz</a>
  <sup>🔗</sup>
- Quarto version 1.6.39 or higher -
  <a href="https://quarto.org/docs/get-started/"
  target="_blank">Quarto</a> <sup>🔗</sup>. Since we use the latest
  Quarto functionalities, you will need at least this version.

**Github, GitHub Desktop**

- A Github account and (optionally) GitHub Desktop version 3.4.16 - <a
  href="https://docs.github.com/en/desktop/installing-and-authenticating-to-github-desktop/installing-github-desktop"
  target="_blank">Github</a> <sup>🔗</sup>

**GFortran**

- GFortran -
  <a href="https://fortran-lang.org/learn/os_setup/install_gfortran/"
  target="_blank">Fortran</a> <sup>🔗</sup>. Installation will take
  several minutes.

**R packages, fonts**

- All other packages are installed automatically by running the project
  `_Setup.R` file on your local machine. Instructions are below.
- The installation of the Liter font is optional. Follow the
  instructions from Google to install this font. -
  <a href="https://fonts.google.com/specimen/Liter"
  target="_blank">Google</a> <sup>🔗</sup>

### Installing

1.  First, make a fork of the repository to your own Github account.
    Then clone the repository to your local machine using for instance
    GitHub Desktop. See <a
    href="https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository"
    target="_blank">Cloning a repository</a> <sup>🔗</sup> for an
    explanation.
2.  Once you have cloned the repository, open the project in RStudio and
    run the `_Setup.R` file by clicking on ‘Source’. Don’t change any
    settings yet. This action will install all the necessary packages
    and fonts.
3.  Ignore any inline suggestions at the top of R Studio to install
    missing packages. Click on `Don't show again`
4.  When presented with the following question, choose 1.

``` plaintext
It looks like you've called renv::restore() in a project that hasn't been activated yet.
How would you like to proceed? 

1: Activate the project and use the project library.
2: Do not activate the project and use the current library paths.
3: Cancel and resolve the situation another way.
```

## 📚 Rendering your first fairness reports

### Render the `_advanced-report` using the default settings

First, run the `_advanced-report` using the default settings.

- To start with, you will create an extended report for The Hague
  University of Applied Sciences (THUAS) with the default settings. It
  contains detailed information about the fairness analysis: prediction
  models, R-code and an explanation of each step with references.

**To render the report, follow these steps:**

Since the template has several dependencies, you must render the pages
using the terminal. Do not render the pages using the Render button
within RStudio.

1.  Open a terminal in RStudio (second tab at the bottom of your
    screen).
2.  Run the following command for the \_advanced-report:

``` r
quarto render --profile advanced-report
```

3.  The first time this report is generated, the process may take 5–10
    minutes. This is normal—please do not interrupt it and allow it to
    complete.
4.  The report is based on synthesized data from the bachelor’s
    Communication & Multimedia Design (CMD), and will be created in the
    `_advanced-report` folder of your project. Once finished, click on
    `_advanced-report/index.html` and choose `View in Web Browser`.
5.  If you don’t see this file, click the refresh icon at the top right
    of the folder. If you still don’t see the report, check the console
    for errors.
6.  When you rerun the report, the output will be removed and a new
    report will be created. If you want to keep the previously rendered
    output, copy the entire folder to a location outside of your
    project.

### Render the `_basic-report` project using the default settings

Next, run the `_basic-report` using the default settings.

- Again, you will create a report for The Hague University of Applied
  Sciences (THUAS) with the default settings. The report is based on
  synthesized data from the bachelor’s Communication & Multimedia Design
  (CMD), and will be created in the `_basic-report` folder of your
  project.

**To render the report, follow these steps:**

Since the template has several dependencies, you must render the pages
using the terminal. Do not render the pages using the Render button
within RStudio.

1.  Open a terminal in RStudio (second tab at the bottom of your
    screen).
2.  Run the following command for the *basic* report:

``` r
quarto render --profile basic-report
```

3.  The first time this report is generated, the process may take about
    5 minutes. This is normal—please do not interrupt it and allow it to
    complete.
4.  Once the rendering has finished, click on `_basic-report/index.html`
    and choose `View in Web Browser`. The report will open in your
    default browser.
5.  If you don’t see this file, click the refresh icon at the top right
    of the folder. If you still don’t see the report, check the console
    for any errors.
6.  When you rerun the report, the output will be removed and a new
    report will be created. If you want to keep the previously rendered
    output, copy the entire folder to a location outside of your
    project.
7.  This report uses the freeze option, which means htmls items will be
    saved, unless one of your sources files change. This will save you
    time, when you rerender the report. Freeze files are stored in the
    `_freeze` folder. If you want to start fresh, simply remove the
    `_freeze` folder.

### Create and render your institutional reports

Finally, you can create your institutional report by changing the
settings in the `_quarto.yml` file.

- You can change the settings for the layout, the educational
  institution, faculty or academy, the study program, the report’s
  version, the success factors, the model, the use of synthetic data,
  the recreation of plots, the enrollment selection and the includes.
- Start small and work your way up to more complex settings.
- Follow the [Tutorial](TUTORIAL.md) to learn how to create and render
  your institutional reports.

## 🏁 Deployment

The reports are created in the `_basic` and `advanced` folders. You can
deploy the reports to a web server or share them with others by copying
the folder to a location outside your project. You can also share the
reports by uploading them to a web server or by using a service like
GitHub Pages.

## 💡 Learn more on fairness, awareness, and equity

To learn more about the concepts of Fairness, Awareness, and Equity,
read the inaugural speech
<a href="https://zenodo.org/records/14204674" target="_blank">No
Fairness without Awareness</a> <sup>🔗</sup> by Theo Bakker.

## 🔨 Built with

- <a href="https://quarto.org/docs/get-started/"
  target="_blank">Quarto</a> <sup>🔗</sup> - The web framework used with
  R.
- <a href="https://www.tidyverse.org/" target="_blank">Tidyverse</a>
  <sup>🔗</sup> - The R coding standard used.
- <a href="https://www.tidymodels.org/" target="_blank">Tidymodels</a>
  <sup>🔗</sup> - The R package used to build the prediction models.
- <a href="https://dalex.drwhy.ai/" target="_blank">Dalex</a>
  <sup>🔗</sup> - The R package used to explain the prediction models
  and create the fairness analysis. The implementation of the fairness
  analysis is based on the
  <a href="https://fairness.drwhy.ai/" target="_blank">Fairness</a>
  <sup>🔗</sup> package.

The LTA-HHs team adjusted the Dalex implementation to improve usability
and make it suitable for use in the educational sector in the
Netherlands.

## 🔧 Contributing

In the next version of this template, we will explain how you can
contribute to this project. Until then, please get in touch with Theo
Bakker if you have any suggestions.

<!-- ## 🔩 Versioning -->

<!-- We use [SemVer](http://semver.org/) for versioning. See the [tags on this repository](https://github.com/LTA-HHs/lta-hhs-fairnessawareness/tags) for the available versions.  -->

## ✒️ Authors

- **Theo Bakker**, Professor of Learning Technology & Analytics of The
  Hague University of Applied Sciences (THUAS), t.c.bakker@hhs.nl -
  *Initial work* -
  <a href="https://github.com/tcbakker" target="_blank">Theo Bakker</a>
  <sup>🔗</sup> and
  <a href="https://github.com/LTA-HHs" target="_blank">LTA-HHs</a>
  <sup>🔗</sup>

## 🚦 License

This project is licensed under the [Creative Commons Attribution Share
Alike 4.0 International](LICENSE.md).

## 🔆 Acknowledgments

- We want to thank the developers of Quarto, Tidyverse, Tidymodels and
  Dalex for their excellent work.
- We want to thank Npuls and CEDA for their support in developing this
  template.

## 📌 Known issues

- None

## 📎 Wishes

- Be able to include logo in \_brand.yml for subsequent use in
  basic.scss.
- Remove Warnings while rendering .qmd files without parameters

## 📫 Contact

If you have any questions or suggestions, please contact the author of
this template: [Theo Bakker](mailto:t.c.bakker@hhs.nl)

*To the [version history of the template](NEWS.md).*

[Back to top](#top)
