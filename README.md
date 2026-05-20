# River Hypoxia Data and R Scripts

This repository contains the processed data and R scripts used to reproduce the main figures and selected supplementary analyses for the manuscript on river hypoxia dynamics.

## Repository structure

- `Figure_1/`  
  Processed data and R scripts used to reproduce Figure 1.

- `Figure_2/`  
  Processed data and R scripts used to reproduce Figure 2.

- `Figure_3/`  
  Processed data and R scripts used to reproduce Figure 3.

- `Figure_4/`  
  Processed data and R scripts used to reproduce Figure 4.

- `Figure_S4/`  
  Processed data and R scripts used to reproduce Supplementary Figure S4.

- `GAM/`  
  Data and R scripts used for generalized additive model analyses.

- `land_use_classification/`  
  Data and scripts used for land-use classification and grouping.

## Data

The CSV files in each folder are processed data used for figure generation and statistical analysis. Raw water-quality and environmental data were obtained from the sources described in the manuscript.

## Code

The R scripts in each folder can be used to reproduce the corresponding figures or analyses.

Data processing, statistical analysis, and figure generation were conducted using R version 4.5.2.

## Required R packages

The main R packages used include:

- tidyverse
- ggplot2
- dplyr
- readr
- sf
- terra
- mgcv

Additional packages may be required for specific figures and are loaded at the beginning of each R script.

## Usage

Download or clone this repository, open the relevant R script, and run it in R or RStudio. Each script is organized to read the processed data from the same folder or its subfolders.

## Citation

If using these data or scripts, please cite the associated manuscript.

## Contact

For questions about the data or code, please contact the corresponding author of the manuscript.
