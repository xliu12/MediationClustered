
# Outcome -------------------------

y.m1m2ac <- function(data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm", interaction = c("AM.M1", "AM.M2", "Mint", "AMint"), 
                     folds, learners, bounded = FALSE) {
    
    vals <- expand.grid(m1 = c(0,1), m2 = c(0,1), a = c(0,1))
    vals_m2a <- expand.grid(m2 = c(0,1), a = c(0,1))
    vals_m1a <- expand.grid(m1 = c(0,1), a = c(0,1))
    aa_vals <- c(0, 1)
    namevec <- c(glue("y_m1m2a({vals$m1},{vals$m2},{vals$a}|c)"), 
                            "y_m1m2a(obs,obs,obs|c)", 
                            glue("y_m1m2a(obs,{vals_m2a$m2},{vals_m2a$a}|c)"), 
                            glue("y_m1m2a({vals_m1a$m1},obs,{vals_m1a$a}|c)"),
                            glue("y_m1m2a(obs,obs,{aa_vals}|c)")
                            )
    y_m1m2ac <- matrix(nrow = nrow(data_in), ncol = length(namevec))
    colnames(y_m1m2ac) <- namevec
    
    if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
      folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
      folds[[1]]$training_set <- folds[[1]]$validation_set 
    }
    
    v <- 1
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        
        valid_list <- lapply(1:nrow(vals), function(m1m2a = 1) {
            valid_m1m2a <- origami::validation(data_in, folds[[v]])
            valid_m1m2a[, c(varnames$M, varnames$A)] <- sapply(vals[m1m2a, ], rep, length = nrow(valid_m1m2a))
            valid_m1m2a
        })
        names(valid_list) <- glue("m1m2a({vals$m1},{vals$m2},{vals$a}|c)")
        valid_list[["m1m2a(obs,obs,obs|c)"]] <- origami::validation(data_in, folds[[v]])
        
        valid_list_m2a <- lapply(1:nrow(vals_m2a), function(m2a = 1) {
            valid_m2a <- origami::validation(data_in, folds[[v]])
            valid_m2a[, c(varnames$M[2], varnames$A)] <- sapply(vals_m2a[m2a, ], rep, length = nrow(valid_m2a))
            valid_m2a
        })
        names(valid_list_m2a) <- glue("m1m2a(obs,{vals_m2a$m2},{vals_m2a$a}|c)")
        
        valid_list_m1a <- lapply(1:nrow(vals_m1a), function(m1a = 1) {
            valid_m1a <- origami::validation(data_in, folds[[v]])
            valid_m1a[, c(varnames$M[1], varnames$A)] <- sapply(vals_m1a[m1a, ], rep, length = nrow(valid_m1a))
            valid_m1a
        })
        names(valid_list_m1a) <- glue("m1m2a({vals_m1a$m1},obs,{vals_m1a$a}|c)")
        
        
        valid_list_a <- lapply(aa_vals, function(a_val = 1) {
            valid_a <- origami::validation(data_in, folds[[v]])
            valid_a[, c(varnames$A)] <- a_val
            valid_a
        })
        names(valid_list_a) <- glue("m1m2a(obs,obs,{aa_vals}|c)")
        
        # make sure the order is the same as colnames(y_m1m2ac)
        y_valid_list <- c(valid_list, valid_list_m2a, valid_list_m1a, valid_list_a)
        valid_listup <- lapply(y_valid_list, update.interactions, varnames=varnames)
        
        alist <- crossfit(train, valid_listup, 
                          varnames$Y, 
                          c(varnames$M, varnames$A, interaction, varnames$X), 
                          varnames, 
                          ipw,
                          cluster_opt, 
                          type = Yfamily, 
                          learners, bounded)
        # alist$fit$fitLibrary$SL.glm_All$object$coefficients
        preds <- alist$preds
        # paste("y_", colnames(preds), sep = "") == colnames(y_m1m2ac)
        y_m1m2ac[folds[[v]]$validation_set, ] <- preds
        
    }
    y_m1m2ac
}

# integrate over p(m2 | a2, c)
# \mu_{M_2}(M_1, a, C) & =\sum_{m_2} \mu(M_1, m_2, a, C) \mathrm{p}(m_2 \mid a_2, C)
y.M1a0c <- function(y_m1m2ac, a0 = 1, m2_ac, a2 = 0) {
    vals_m2 <- c(0, 1)
    y_M1a0c <- rowSums(y_m1m2ac[, glue("y_m1m2a(obs,{vals_m2},{a0}|c)")] * m2_ac[, glue("m2({vals_m2}|{a2},c)")])
    y_M1a0c
}


# integrate over p(m1 | a1, c)
# \mu_{M_1}(M_2, a, C) & =\sum_{m_1} \mu(m_1, M_2, a, C) \mathrm{p}(m_1 \mid a_1, C)
y.M2a0c <- function(y_m1m2ac, a0 = 1, m1_ac, a1 = 0) {
    vals_m1 <- c(0, 1)
    y_M2a0c <- rowSums(y_m1m2ac[, glue("y_m1m2a({vals_m1},obs,{a0}|c)")] * m1_ac[, glue("m1({vals_m1}|{a1},c)")])
    y_M2a0c
}

# integrate over p(m1 | a1, c)*p(m2 | a2, c)
y.a0c_12 <- function(y_m1m2ac, a0 = 1, m1_ac, a1 = 0, m2_ac, a2 = 0) {
    vals_m1m2 <- expand.grid(m1 = c(0, 1), m2 = c(0, 1))
    y_a0c <- rowSums( y_m1m2ac[, glue("y_m1m2a({vals_m1m2$m1},{vals_m1m2$m2},{a0}|c)")] * 
            m1_ac[, glue("m1({vals_m1m2$m1}|{a1},c)")] * 
            m2_ac[, glue("m2({vals_m1m2$m2}|{a2},c)")] )
    y_a0c
}



# integrate over p(m1, m2 | ajo, c)
y.a0c_jo <- function(y_m1m2ac, a0 = 1, m1m2_ac, ajo = 0) {
    vals_m1m2 <- expand.grid(m1 = c(0, 1), m2 = c(0, 1))
    y_a0c <- rowSums( y_m1m2ac[, glue("y_m1m2a({vals_m1m2$m1},{vals_m1m2$m2},{a0}|c)")] * 
                          m1m2_ac[, glue("m1m2_ac({vals_m1m2$m1},{vals_m1m2$m2}|{ajo},c)")] )
    y_a0c
}
