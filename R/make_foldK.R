

# within cluster sample split and cross fit 

make.fold_K <- function(data_in, Sname, cv_folds=4) {
  
  fold_K <- lapply(unique(data_in[[Sname]]), FUN = function(k=1) {
    data_in$K <- match(data_in[[Sname]], unique(data_in[[Sname]]))
    if (nrow(data_in[data_in$K==k, ]) >= 1) {
      fk <- origami::make_folds(data_in[data_in$K==k, ],
                                fold_fun = origami::folds_vfold,
                                V = cv_folds)
      fold_k <- fk
      v <- 1
      for(v in 1:cv_folds) {
        fold_k[[v]]$validation_set <- data_in$id[data_in$K==k][fk[[v]]$validation_set]
        fold_k[[v]]$training_set <- data_in$id[data_in$K==k][fk[[v]]$training_set]
      }
    }
    
    # if (nrow(data_in[data_in$K==k, ]) < 4) {
    #   # if cluster size too small, no cluster split; use entire cluster as both training and valid
    #   fk <- origami::make_folds(
    #     data_in[data_in$K==k, ][sample(1:nrow(data_in[data_in$K==k, ]), cv_folds*2, replace = T), ],
    #                             fold_fun = origami::folds_vfold,
    #                             V = cv_folds
    #   )
    #   fold_k <- fk
    #   for(v in 1:cv_folds) {
    #     fold_k[[v]]$validation_set <- data_in$id[data_in$K==k]
    #     fold_k[[v]]$training_set <- data_in$id[data_in$K==k]
    #   }
    #
    # }
    
    return(fold_k)
  } )
  
  folds <- origami::make_folds(data_in,
                               fold_fun = origami::folds_vfold,
                               V = cv_folds)
  
  for(v in 1:cv_folds) {
    folds[[v]]$validation_set <- unlist(lapply(1:length(fold_K), FUN = function(k=1) {
      fold_K[[k]][[v]]$validation_set
    }))
    folds[[v]]$training_set <- unlist(lapply(1:length(fold_K), FUN = function(k=1) {
      fold_K[[k]][[v]]$training_set
    }))
  }
  
  folds
}
