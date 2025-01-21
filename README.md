# MediationClustered
R package for estimating the interventional direct and indirect effects for multiple mediators with clustered data, using the multiply robust method incorporating machine learning approaches. 

For details, please see the accompanying methods paper:

Liu, X. (2024). Estimating Causal Mediation Effects in Multiple-Mediator Analyses with Clustered Data. Journal of Educational and Behavioral Statistics. Forthcoming.

## Installation

To install `MediationClustered` directly from Github:
```
remotes::install_github("xliu12/MediationClustered")
```

## Example

```
data("data_example")

library(SuperLearner)
library(origami)
library(glue)
library(tidyverse)
library(fastDummies)

# cluster indicator
Sname <- "school"

# individual-level covariates
Xnames <- colnames(data_example)[grep("^X", colnames(data_example))]

# cluster-level covariates
Wnames <- colnames(data_example)[grep("^W", colnames(data_example))]

# treatment variable
Aname <- colnames(data_example)[grep("^A_", colnames(data_example))]

# mediators
Mnames <- colnames(data_example)[grep("^M_", colnames(data_example))]

# outcome variable
Yname <- colnames(data_example)[grep("^Y_", colnames(data_example))]

# estimation methods for models of the treatment, mediators, and outcome chosen from R package \code{SuperLearner}
learners_a <- learners_m <- learners_y <- c("SL.gam", "SL.glm", "SL.nnet")

out <- MediationClustered::ClusterMed(data = data_example,
                 Sname = Sname,
                 Wnames = Wnames,
                 Xnames = Xnames,
                 Aname = Aname,
                 Mnames = Mnames,
                 Yname = Yname,
                 learners_a = learners_a,
                 learners_m = learners_m,
                 learners_y = learners_y
)

out

```
