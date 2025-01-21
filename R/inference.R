

get.inference <- function(estimand=1, eif_clmean, sizes, average = "individual") {
  K <- length(sizes)
  mean_size <- sum(sizes)/K
  
  if (average=="individual") {
    estimate <- sum(eif_clmean[1:K, estimand] * sizes/mean_size) / K
    variance <- var(eif_clmean[1:K, estimand] * sizes/mean_size)/K
  }
  
  if (average=="cluster") {
    estimate <- sum(eif_clmean[1:K, estimand])/K # mean(eif_clmean[, estimand])
    variance <- var(eif_clmean[1:K, estimand])/K
  }
  
  c(est=estimate, se=sqrt(variance))
  
}