# fit a model


crossfit <- function(train, valid.list, yname, xnames, varnames,
                     ipw = NULL,
                     cluster_opt = "FE.glm",
                     type, learners, bounded = FALSE) {
    
    Sname <- varnames$S
    Sname_dummies <- varnames$Sdumm
    
    family <- ifelse(type == "binomial", binomial(), gaussian())
    
    df_lm <- data.frame(Y = train[[yname]],
                        train[, c(xnames, varnames$W), drop = FALSE],
                        S = train[[Sname]] )
    if (length(ipw) > 0) {
        df_lm$wreg <- ipw
    }
    if (length(ipw) == 0) {
        df_lm$wreg <- rep(1, nrow(df_lm))
    }
    
    # random effects ----
    if (cluster_opt == "RE.glm") {
      REformula <- paste("Y ~", paste(c(xnames, varnames$W), collapse = " + "), "+ (1 | S)")
      if (family[[1]] == "gaussian") {
        fit <- lmer(formula = REformula,
                    weights = wreg,
                    data = df_lm)
      }
      if (family[[1]] != "gaussian") {
        fit <- glmer(formula = REformula,
                     weights = wreg,
                     data = df_lm, family = family[[1]])
      }
      preds <- sapply(valid.list, function(validX) {
        newX <- data.frame(validX[, c(xnames, varnames$W), drop = FALSE],
                           S = validX[[Sname]] )
        preds <- predict(fit, newX, type = "response")
        if (!bounded) {
          return(preds)
        }
        bound(preds)
      }, simplify = TRUE)
      
    }
    
    # fixed effects ----
    
    ## glm ----
    if (cluster_opt %in% c("FE.glm", "noncluster.glm")) {
        if (cluster_opt == "FE.glm") {
            fit <- glm(formula = paste("Y ~ S +", paste(xnames, collapse = " + ")),
                       weights = wreg,
                       data = df_lm, family = family[[1]])
        }
        if (cluster_opt == "noncluster.glm") {
            fit <- glm(formula = paste("Y ~", paste(xnames, collapse = " + ")),
                       weights = wreg,
                       data = df_lm, family = family[[1]])
        }
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, xnames, drop = FALSE],
                               S = validX[[Sname]] )
            preds <- predict(fit, newX, type = "response")
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    ## dummy cluster indicator with SL ----
    if (cluster_opt %in% c("fix.mlr", "noncluster.mlr")) {
        df_FE <- data.frame(df_lm, train[, Sname_dummies])
        set.seed(12345)
        if (cluster_opt == "fix.mlr") {
            
            fit <- SuperLearner::SuperLearner(
                df_FE$Y,
                df_FE[, c(xnames, varnames$W, Sname_dummies), drop = FALSE],
                obsWeights = df_FE$wreg,
                family = family[[1]],
                SL.library = learners
            )
        }
        if (cluster_opt == "noncluster.mlr") {
            fit <- SuperLearner::SuperLearner(
                df_FE$Y,
                df_FE[, c(xnames), drop = FALSE],
                obsWeights = df_FE$wreg,
                family = family[[1]],
                SL.library = learners
            )
        }
        
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, c(xnames, varnames$W, Sname_dummies), drop = FALSE])
            preds <- predict(fit, newX[, fit$varNames])$pred
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    
    
    # cwc with SL ------------------------
    if (cluster_opt == "cwc") { # continuous outcome
      # colnames(train) # note: train[, glue("{yname}"), drop=TRUE] doesn't work well
      df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
                           train[, c(glue("{xnames}_cwc"), glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
                                     #glue("{xnames}_clmean"),
                                     varnames$W
                           ), drop = FALSE])
      
      fit <- SuperLearner::SuperLearner(
        df_cwc$Y,
        df_cwc[, -1, drop = FALSE],
        family = family[[1]],
        SL.library = learners,
        env = environment(SuperLearner::SuperLearner)
      )
      
      preds <- sapply(valid.list, function(validX) {
        
        newX <- data.frame(validX[, c(xnames, glue("{xnames}_clmean"), glue("{yname}_clmean"), varnames$W), drop = FALSE]) 
        validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
        colnames(validX_cwc) <- glue("{xnames}_cwc")
        newX <- newX %>% bind_cols(validX_cwc)
        # colnames(newX)
        preds <- predict(fit, newX[, fit$varNames])$pred
        # preds <- preds + validX[, glue("{yname}_clmean"), drop=TRUE]
        
        if (!bounded) {
          return(preds)
        }
        bound(preds)
      }, simplify = TRUE)
      
      
    }
    
    # sufficient stats ----
    if (cluster_opt == "sufficient_stats")  { # continuous outcome
      if (family[[1]] == "binomial") {
        df_ss <- data.frame(Y = train[, glue("{yname}"), drop=TRUE],
                            train[,c(glue("{xnames}_cwc"), glue("{xnames}_clmean"), glue("{yname}_clmean"), 
                                     varnames$W), drop = FALSE])
      }
      if (family[[1]] == "gaussian") {
        df_ss <- data.frame(Y = train[, glue("{yname}_cwc"), drop=TRUE],
                            train[,c(glue("{xnames}_cwc"), glue("{xnames}_clmean"), #glue("{yname}_clmean"), 
                                     varnames$W), drop = FALSE])
      } 
      
      fit <- SuperLearner::SuperLearner(
        df_ss$Y,
        df_ss[, -1, drop = FALSE],
        family = family[[1]],
        SL.library = learners,
        env = environment(SuperLearner::SuperLearner)
      )
      
      preds <- sapply(valid.list, function(validX) {
        
        newX <- data.frame(validX[, c(glue("{yname}_clmean"), xnames, glue("{xnames}_clmean"), varnames$W), drop = FALSE]) 
        validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
        colnames(validX_cwc) <- glue("{xnames}_cwc")
        newX <- newX %>% bind_cols(validX_cwc)
        # colnames(newX)
        preds <- predict(fit, newX[, fit$varNames])$pred
        
        if (family[[1]] == "gaussian") {
          preds <- preds + validX[, glue("{yname}_clmean"), drop=TRUE]
        }
        
        if (!bounded) {
          return(preds)
        }
        bound(preds)
      }, simplify = TRUE)
      
      
    }
    
    # cwc.FE ----
    if (cluster_opt == "cwc.FE") { # continuous outcome
      # colnames(train) # note: train[, glue("{yname}"), drop=TRUE] doesn't work well
      if (family[[1]]=="gaussian") {
        df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
                             train[, c(glue("{xnames}_cwc"), Sname_dummies, glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
                                       #glue("{xnames}_clmean"),
                                       varnames$W
                             ), drop = FALSE])
      }
      if (family[[1]]=="binomial") {
        df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
                             train[, c(glue("{xnames}_cwc"), Sname_dummies, glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
                                       #glue("{xnames}_clmean"),
                                       varnames$W
                             ), drop = FALSE])
      }
      
      
      fit <- SuperLearner::SuperLearner(
        df_cwc$Y,
        df_cwc[, -1, drop = FALSE],
        family = family[[1]],
        SL.library = learners,
        env = environment(SuperLearner::SuperLearner)
      )
      
      preds <- sapply(valid.list, function(validX) {
        
        newX <- data.frame(validX[, c(xnames, glue("{xnames}_clmean"), glue("{yname}_clmean"), Sname_dummies, varnames$W), drop = FALSE]) 
        validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
        colnames(validX_cwc) <- glue("{xnames}_cwc")
        newX <- newX %>% bind_cols(validX_cwc)
        # colnames(newX)
        preds <- predict(fit, newX[, fit$varNames])$pred
        
        if (family[[1]]=="gaussian") {
          preds <- preds #+ validX[, glue("{yname}_clmean"), drop=TRUE]
        }
        
        
        if (!bounded) {
          return(preds)
        }
        bound(preds)
      }, simplify = TRUE)
      
      
    }
    
    out <- list(fit = fit, preds = preds)
    return(out)
    

# old (original submission)
# if (cluster_opt == "FE.mlr") {

# colnames(df_cwc)
# df_FE <- data.frame(df_lm, train[, Sname_dummies])
# df_cwc <- df_lm %>%
#     group_by(S) %>%
#     mutate(across(everything(), ~.x-mean(.x) ))
# df_cluster <- data.frame(df_cwc, train[, Sname_dummies])

# fit <- SuperLearner::SuperLearner(
#     df_FE$Y,
#     df_cluster[, c(xnames, Sname_dummies), drop = FALSE],
#     family = family[[1]],
#     SL.library = learners,
#     env = environment(SuperLearner::SuperLearner)
# )
# preds <- sapply(valid.list, function(validX) {
#     newX <- data.frame(Y = validX[[yname]],
#                        validX[, xnames, drop = FALSE],
#                        S = validX[[Sname]] )
#     newX_cwc <- newX %>%
#         group_by(S) %>%
#         mutate(across(everything(), ~.x-mean(.x) ))
#     newX_cluster <- data.frame(newX_cwc, validX[, Sname_dummies])
#     
#     preds <- predict(fit, newX_cluster[, fit$varNames])$pred
#     
#     if (!bounded) {
#         return(preds)
#     }
#     bound(preds)
# }, simplify = TRUE)
# }

    
}



bound <- function(vals, tol = 0.025) {
    vals[vals < tol] <- tol
    vals[vals > 1 - tol] <- 1 - tol
    return(vals)
}

bound.precison <- function(vals, tol = 1e-4) {
  vals[vals < tol] <- tol
  vals[vals > 1 - tol] <- 1 - tol
  return(vals)
}


SL.xgboost.modified <- function(...) {
  SL.xgboost(..., ntrees = 100)
}
SL.ranger.modified <- function(...) {
  SL.ranger(...,num.trees = 200)
}

SL.gam.modified <- function (Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4, 
                             ...) 
{
  Xoffset <- NULL
  if (length(grep("_clmean", colnames(X), value = T))>0) {
    Xoffset <- X[,grep("_clmean", colnames(X), value = T)]
    if (family=="binomial") {
      Xoffset <- bound.precision(qlogis(Xoffset))
    }
    # Xoffset <- grep("_clmean", colnames(X), value = T)
  }
 X <- X[, !grepl("_clmean", colnames(X))]
  
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(!cts.x) > 0) {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+"), "+", paste(colnames(X[, 
                                                                                                           !cts.x, drop = FALSE]), collapse = "+")))
  }
  else {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+")))
  }
  if (sum(!cts.x) == length(cts.x)) {
    gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                                              collapse = "+"), sep = ""))
  }
  fit.gam <- gam::gam(gam.model, data = data.frame(X, Y=Y), family = family, offset=Xoffset,
                      control = gam::gam.control(maxit = 50, bf.maxit = 50), 
                      weights = obsWeights)
  if (packageVersion("gam") >= "1.15") {
    pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
  }
  else {
    stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  }
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gam")
  return(out)
}
