#' Estimating Causal Mediation Effects in Multiple-Mediator Analyses with Clustered Data
#' @param data a \code{data.frame} containing the analysis variables.
#' @param Sname a character string of the name of the column in "data" that indicates the cluster membership of each individual (S).
#' @param Wnames a character vector of the names of the columns in "data" that correspond to cluster-level baseline (i.e., pre-treatment) covariates (W). "Wnames" may include the cluster size (i.e., number of individuals in each cluster).
#' @param Xnames a character vector of the names of the columns in "data" that correspond to individual-level baseline covariates (X).
#' @param Aname a character string of the name of the column in "data" that corresponds to a dummy coded treatment variable (A).
#' @param Mnames a character vector of the names of the columns in "data" that correspond to mediators (M).
#' @param Yname a character string of the name of the column in "data" that corresponds to an outcome variable (Y).
#' @param cluster_opt a character string of the option of incorporating clusters in the nuisance model estimation. Currently supported options are "cwc.FE" (the default option) and "cwc". "cwc.FE": Control for cluster means  and perform cluster-mean centering for individual-level variables, and also control for dummy indicators of an individual's  cluster membership. "cwc": Control for cluster means  and perform cluster-mean centering for individual-level variables (while omitting the cluster dummies).
#' @param learners_a a character vector specifying estimation methods for the model of the treatment For "learners_a", "learners_m", and "learners_y", currently supported options include methods in the "SuperLearner" package (<https://cran.r-project.org/package=SuperLearner>), available via the "SuperLearner::listWrappers()".
#' @param learners_m a character vector specifying estimation methods for the models of the mediators.
#' @param learners_y a character vector specifying estimation methods for the model of the outcome.
#' @param num_folds the number of folds to use for the cross-fitting procedure.
#'
#' @return A data frame containing the results (estimates and 0.95 confidence intervals) for each of the cluster-average and individual-average causal mediation effects (i.e., IDE, IIE_M1, IIE_M2, IIE_mu, including the cluster-average and individual-average versions).
#' IDE, IIE_M1, IIE_M2, and IIE_mu are, respectively, the interventional direct effect, indirect effect via M_1 (the first mediator in "Mnames"), indirect effect via M_2 (the second mediator in "Mnames"), and indirect effect due to the mediators' mutual dependence.

#'
#'
#' @export
#'

ClusterMed <- function(data,
                       Sname,
                       Wnames, Xnames,
                       Aname,
                       Mnames,
                       Yname,
                       learners_a = c("SL.glm"),
                       learners_m = c("SL.glm"),
                       learners_y = c("SL.glm"),
                       cluster_opt = "cwc.FE",
                       Morder = "12",
                       num_folds = 1
) {
    set.seed(12345)

    Yfamily <- ifelse(length(unique(data[[Yname]]))>2, "gaussian", "binomial")

    out <- clustered(data = data,
              Sname = Sname,
              Wnames = Wnames, Xnames = Xnames,
              Aname = Aname,
              Mnames = Mnames, Mfamily = "binomial",
              Yname = Yname, Yfamily = Yfamily,
              cluster_opt_a = cluster_opt,
              cluster_opt_m = cluster_opt,
              cluster_opt_y = cluster_opt,
              Morder = Morder,
              num_folds = num_folds,
              learners_a = learners_a,
              learners_m = learners_m,
              learners_y = learners_y
    )

    results <- out$mr_results
    results1 <- results %>%
        filter(str_detect(estimand, "^I")) %>%
        filter(!str_detect(estimand, "Mjo")) %>%
        mutate(Effect = factor(case_when(str_detect(estimand, "IDE") ~ "IDE",
                                         str_detect(estimand, "IIE_M1") ~ "IIE_M1",
                                         str_detect(estimand, "IIE_M2") ~ "IIE_M2",
                                         str_detect(estimand, "IIE_Mdep") ~ "IIE_mu"),
                               levels = c("IDE", "IIE_M1", "IIE_M2","IIE_mu"))
               ) %>%
        select(Effect, starts_with("mr_")) %>%
        rename_with(.fn = ~gsub("mr_", "", .) ) %>%
        rename_with(.fn = ~gsub("individual", "Individual.Average", .) ) %>%
        rename_with(.fn = ~gsub("cluster", "Cluster.Average", .) ) %>%
        rename_with(.fn = ~gsub(".est", "_Estimate", .) ) %>%
        rename_with(.fn = ~gsub(".se", "_StdError", .) ) %>%
        rename_with(.fn = ~gsub("waldci1_", "CI.lower_", .) ) %>%
        rename_with(.fn = ~gsub("waldci2_", "CI.upper_", .) ) %>%
        select(Effect, contains("Individual.Average"), contains("Cluster.Average"))

    results1
}
