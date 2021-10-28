# Native Land Research Initiative

This repository contains the code (and some data) to produce the appendix for the paper "Effects of land dispossession and forced migration on Indigenous peoples in North America."

Justin Farrell Paul Burow Kathryn McConnell Jude Bayham Kyle Whyte Gal Koss

********************************************

# Project and directory structure

This section describes the directory structure of the project and how to build outputs describing results of the analysis.  The root contains a data dictionary to help interpret the fields in the datasets stored at https://osf.io/3cfum/.  The code and data to build the supplementary material (Appendix A) and other analysis are contained in [03_analysis](../main/03_analysis/).


- [01_appendix_A.Rmd](../main/03_analysis/01_appendix_A.Rmd) is an Rmarkdown document with the text and code to produce the appendix.  We include a copy of the compiled appendix [01_appendix_A.pdf](../main/03_analysis/01_appendix_A.pdf).  The Rmd file loads function contained in [support_functions.R](../main/03_analysis/support_functions.R) and the five files with a data_ prefix with .rds extensions.

- [dyad_analysis.R](../main/03_analysis/dyad_analysis.R) contains the code to calculate the distance between historical and present-day lands.

- [land_area_change.R](../main/03_analysis/land_area_change.R) contains the code to calculate the change in land area between the historical and present-day periods.

