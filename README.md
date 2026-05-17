# CAPS-ACSP COVID-19 Survey Analysis

This repository contains the code and supporting materials used to clean, analyze, and report on COVID-19 survey responses collected by the Canadian Association of Postdoctoral Scholars (CAPS-ACSP) from the postdoctoral community in Canada.

## Project Overview

The analyses in this repository focus on two survey initiatives conducted during the COVID-19 pandemic:

1. **COVID-19 Impact Survey**  
   Responses collected between April 1 and May 31, 2020.

2. **Back-to-Work Survey**  
   Responses collected between June and December 2020.

The repository includes workflows for:
- data cleaning and preprocessing
- statistical analysis
- visualization and mapping
- survey reporting

---

## Repository Structure

```text
caps_surveys_analysis/
├── R/                         # R scripts and functions
├── code/                      # Survey analysis and processing scripts
├── data/                      # Survey datasets and processed data
├── README.md                  # Project documentation
├── LICENSE                    # MIT license
└── caps_surveys_analysis.Rproj # RStudio project file
```

---

## Software Requirements

This project was developed primarily in R using common packages for:
- data manipulation
- statistical analysis
- visualization
- spatial mapping

Example packages may include:

```r
install.packages(c(
  "tidyverse",
  "ggplot2",
  "sf",
  "dplyr",
  "readr"
))
```

---

## Reproducibility

This repository is intended to support transparent and reproducible survey analysis workflows. Scripts are organized to facilitate replication of the analyses and outputs from the survey datasets.

---

## Data Privacy

Survey responses may contain sensitive or confidential information. Raw datasets should not be publicly distributed unless properly anonymized and approved for sharing.

---

## License

This project is licensed under the MIT License.

---

## Acknowledgements

This work was conducted to better understand the impacts of the COVID-19 pandemic on the Canadian postdoctoral research community.
