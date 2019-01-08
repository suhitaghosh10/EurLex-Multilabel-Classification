train_ratio <- 0.65
test_ratio <- 0.35

classify <- function(mldrfilename,
                     mode,
                     classifier,
                     reduceLabels) {
  for (index in 1:batch_number) {
    ds <- mldr(paste(mldrfilename, index, sep = ""))
    if (reduceLabels == TRUE)
      ds <- ds[.SCUMBLE <= ds$measures$scumble]
    ds <- remove_skewness_labels(ds, 1) %>%
      remove_attributes("...") %>%
      remove_unique_attributes() %>%
      remove_unlabeled_instances() %>%
      create_holdout_partition(c(train = train_ratio, test = test_ratio))
    
    if (tolower(mode) == "lp")
      cl_model <- lp(ds$train, classifier)
    else if (tolower(mode) == "cc")
      cl_model <- cc(ds$train, classifier)
    else
      cl_model <- br(ds$train, classifier)
    
    prediction <- tryCatch(
      predict(cl_model, ds$test),
      error = function(e) {
        message(paste("prediction could not be done for ", paste(mldrfilename, index, sep = "")))
        NA
      }
    )
    
    temp <-
      tryCatch(
        multilabel_evaluate(ds$test, prediction, "bipartition"),
        error = function(e) {
          message(paste("evaluation could not be done for ", paste(mldrfilename, index, sep = "")))
          NA
        }
      )
    
    if (index == 1 && !is.na(temp)) {
      if (exists("cls_pred"))
        rm(cls_pred)
      cls_pred <- temp
      print("Classification done for batch 1")
      
    } else if (index > 1 && !is.na(temp)) {
      if(!exists('cls_pred'))
        cls_pred <- temp
      else
      cls_pred <- cbind(cls_pred, temp)
      print(paste("Classification done for batch", index))
    }
    rm(cl_model)
    rm(ds)
    rm(prediction)
    
  }
  
  prediction_mean <- rowMeans(cls_pred, na.rm = TRUE)
  
  prediction_mean
}