# multiStateLangevin-supporting-code
Supporting data and R code for "A multistate Langevin diffusion for inferring behavior-specific habitat selection and utilization distributions" by McClintock &amp; Lander. The main scripts (**simStudy.R** and **sslAnalysis.R**) both depend on the [`Rhabit`](https://github.com/papayoun/Rhabit)  package, which is not available on CRAN. [`RandomFields`](https://cran.r-project.org/package=RandomFields) is imported by `Rhabit`, but it has been removed and archived by CRAN as of 4 May 2022. Thus in order to install the `Rhabit` package, `RandomFields` must be manually [downloaded](https://cran.r-project.org/src/contrib/Archive/RandomFields/RandomFields_3.3.14.tar.gz) and installed beforehand:
```
install.packages("RandomFields_3.3.14.tar.gz", repos = NULL, type = "source")
remotes::install_github("papayoun/Rhabit")
```
The scripts also depend on [`momentuHMM`](https://doi.org/10.1111/2041-210X.12995) version 2.0.0 or higher:
```
remotes::install_github("bmcclintock/momentuHMM@develop",dependencies = TRUE) 
```
## Simulation files
**simStudy.R** contains the simulation experiment.

## Steller seal lion illustration
**sslAnalysis.R** contains the Steller sea lion analysis. All data necessary for running the script are in the **data** folder. Supporting scripts, including model definitions (**DM.R**), spatial covariate processing (**sslCovariates.R**), and other utility functions (**utility.R**) are in the **supportingScripts** folder. **allModels.RData** is the saved workspace containing the results reported in the paper. 
