

# eif 
eif <- function(data_in, varnames, a_c, y_m1m2ac, m1m2_ac, m1_ac, m2_ac) {
  
  a_vals12 <- rbind(
    # IIE_M1, fixing A=1 and p(M2(0) | c)
    expand.grid(a0 = c(1), a1 = c(0, 1), a2 = c(0)),
    # IIE_M2, fixing A=1 and p(M1(1) | c)
    expand.grid(a0 = c(1), a1 = c(1), a2 = c(0, 1))
  )
  a_vals12$ajo <- NA
  a_vals <- rbind(a_vals12,
                  data.frame(a0 = c(1, 1, 0),
                             a1 = NA, a2 = NA,
                             ajo = c(0, 1, 0)))
  
  A <- data_in[[varnames$A]]
  M1 <- data_in[[varnames$M[1]]]
  M2 <- data_in[[varnames$M[2]]]
  Y <- data_in[[varnames$Y]]
  
  # multiply-robust
  thetas <- eifs <- list()
  # regression (G-computation)
  regs <- list()
  # weighting
  rmpw <- list()
  
  i <-1
  for (i in 1:nrow(a_vals)) {
    a0 <- a_vals$a0[i]
    a1 <- a_vals$a1[i]
    a2 <- a_vals$a2[i]
    ajo <- a_vals$ajo[i]
    
    # observed treatment
    # ipwA <- ipwA_c[, glue("ipwA({a0}|c)")]
    p_a0 <- a_c[, glue("a({a0}|c)")]
    ipwA <- (A==a0) / a_c[, glue("a({a0}|c)")]
    # observed outcome
    mu <- y_m1m2ac[, "y_m1m2a(obs,obs,obs|c)"]
    # observed M distribution
    p_Mjo <- (M1==1)*(M2==1)*m1m2_ac[, glue("m1m2_ac(1,1|{a0},c)")] +
      (M1==1)*(M2==0)*m1m2_ac[, glue("m1m2_ac(1,0|{a0},c)")] +
      (M1==0)*(M2==1)*m1m2_ac[, glue("m1m2_ac(0,1|{a0},c)")] +
      (M1==0)*(M2==0)*m1m2_ac[, glue("m1m2_ac(0,0|{a0},c)")]
    # quantile(p_Mjo, c(0, 0.1, 0.9))
    # den <- a_c[, glue("a({a0}|c)")] * p_Mjo
    # quantile(den)
    
    if (!is.na(a1)) {
      p_M1 <- (M1==1)*m1_ac[, glue("m1(1|{a1},c)")] + (M1==0)*m1_ac[, glue("m1(0|{a1},c)")]
      p_M2 <- (M2==1)*m2_ac[, glue("m2(1|{a2},c)")] + (M2==0)*m2_ac[, glue("m2(0|{a2},c)")]
      
      # integrate over p(m2 | a2, c)
      mu_M1a0c <- y.M1a0c(y_m1m2ac, a0, m2_ac, a2)
      # integrate over p(m1 | a1, c)
      mu_M2a0c <- y.M2a0c(y_m1m2ac, a0, m1_ac, a1)
      # integrate over p(m1 | a1, c)*p(m2 | a2, c)
      mu_a0c_12 <- y.a0c_12(y_m1m2ac, a0, m1_ac, a1, m2_ac, a2)
      
      # fot the effect via M1 or M2 alone ----
      # h_M1M2 <- p_M1 * p_M2 / p_Mjo
      # eify <- 1*(A == a0)*ipwA*h_M1M2 / mean(1*(A == a0)*ipwA*h_M1M2) * (Y - mu)
      h_aM1M2 <- p_M1 * p_M2 / bound(p_Mjo*p_a0)
      eify <- 1*(A == a0)*h_aM1M2 / mean(1*(A == a0)*h_aM1M2) * (Y - mu)
      
      ipwa1 <- (A==a1) / bound(a_c[, glue("a({a1}|c)")]) # ipwA_c[, glue("ipwA({a1}|c)")]
      eifm1 <- 1*(A == a1)*ipwa1 / mean(1*(A == a1)*ipwa1) * (mu_M1a0c - mu_a0c_12)
      
      ipwa2 <- (A==a2) / bound(a_c[, glue("a({a2}|c)")]) # ipwA_c[, glue("ipwA({a2}|c)")]
      eifm2 <- 1*(A == a2)*ipwa2 / mean(1*(A == a2)*ipwa2) * (mu_M2a0c - mu_a0c_12)
      
      eifc <- mu_a0c_12 - mean(mu_a0c_12)
      # eif with marginal mediator distributions
      eif_mar <- eify + eifm1 + eifm2 + mu_a0c_12
      
      
      eifs[[glue("Y({a0},gm1({a1}),gm2({a2}))")]] <- eif_mar
      thetas[[glue("Y({a0},gm1({a1}),gm2({a2}))")]] <- mean(eif_mar)
      
      rmpw[[glue("Y({a0},gm1({a1}),gm2({a2}))")]] <- 1*(A == a0)*h_aM1M2 / mean(1*(A == a0)*h_aM1M2) * Y #mean( 1*(A == a0)*h_aM1M2 / mean(1*(A == a0)*h_aM1M2) * Y )
      regs[[glue("Y({a0},gm1({a1}),gm2({a2}))")]] <- mu_a0c_12 #mean(mu_a0c_12)
      
    }
    
    # for the joint ajo ----
    if (!is.na(ajo)) {
      p_Majo <- (M1==1)*(M2==1)*m1m2_ac[, glue("m1m2_ac(1,1|{ajo},c)")] +
        (M1==1)*(M2==0)*m1m2_ac[, glue("m1m2_ac(1,0|{ajo},c)")] +
        (M1==0)*(M2==1)*m1m2_ac[, glue("m1m2_ac(0,1|{ajo},c)")] +
        (M1==0)*(M2==0)*m1m2_ac[, glue("m1m2_ac(0,0|{ajo},c)")]
      # integrate over p(m1, m2 | ajo, c)
      mu_a0c_jo <- y.a0c_jo(y_m1m2ac, a0, m1m2_ac, ajo)
      
      # h_Mjo <- p_Majo / p_Mjo
      # eify_ajo <- 1*(A == a0)*ipwA*h_Mjo / mean(1*(A == a0)*ipwA*h_Mjo) * (Y - mu)
      h_aMjo <- p_Majo / bound(p_Mjo*p_a0)
      eify_ajo <- 1*(A == a0)*h_aMjo / mean(1*(A == a0)*h_aMjo) * (Y - mu)
      
      # fixing A = a0
      mu_M1M2a0c <- y_m1m2ac[, glue("y_m1m2a(obs,obs,{a0}|c)")]
      ipwajo <- (A==ajo) / bound(a_c[, glue("a({ajo}|c)")]) # ipwA_c[, glue("ipwA({ajo}|c)")]
      eifm_ajo <- 1*(A == ajo)*ipwajo / mean(1*(A == ajo)*ipwajo) * (mu_M1M2a0c - mu_a0c_jo)
      
      # eif with the joint mediator distribution
      eif_jo <- eify_ajo + eifm_ajo + mu_a0c_jo
      
      eifs[[glue("Y({a0},gmjo({ajo}))")]] <- eif_jo
      # thetas[[glue("Y({a0},gmjo({ajo}))")]] <- mean(eif_jo)
      
      # rmpw[[glue("Y({a0},gmjo({ajo}))")]] <- mean( 1*(A == a0)*h_aMjo / mean(1*(A == a0)*h_aMjo) * (Y) )
      rmpw[[glue("Y({a0},gmjo({ajo}))")]] <- 1*(A == a0)*h_aMjo / mean(1*(A == a0)*h_aMjo) * (Y) 
      regs[[glue("Y({a0},gmjo({ajo}))")]] <- mu_a0c_jo # mean(mu_a0c_jo)
    }
    
  }
  
  list(eifs=eifs, #eify_ajo=eify_ajo, eifm_ajo=eifm_ajo, mu_a0c_jo=mu_a0c_jo, 
       thetas=thetas, 
       rmpw=rmpw, regs=regs)
}