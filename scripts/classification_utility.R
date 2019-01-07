train_ratio <- 0.65
test_ratio <- 0.35

classify <- function(mldrfilename, mode, classifier, reduceLabels) {
  for (index in 1:batch_number) {
    ds <- mldr(paste(mldrfilename, index, sep = ""))
    if(reduceLabels == TRUE)
      ds <- ds[.SCUMBLE <= ds$measures$scumble]
    ds <- remove_skewness_labels(ds, 1) %>%
      remove_attributes("...") %>%
      remove_unique_attributes() %>%
      remove_unlabeled_instances() %>%
      create_holdout_partition(c(train = train_ratio, test = test_ratio))
    
    if (tolower(mode) == "lp")
      model <- lp(ds$train, classifier)
    else if (tolower(mode) == "cc")
      model <- cc(ds$train, classifier)
    else
      model <- br(ds$train, classifier)
    
    prediction <- predict(model, ds$test)
    temp <- multilabel_evaluate(ds$test, prediction, "bipartition")
    
    rm(model)
    rm(prediction)
    
    if (index == 1) {
      if(exists("cls_pred")) rm(cls_pred)
      cls_pred <- temp
      
    } else{
      cls_pred <- cbind(cls_pred, temp)
    }
    print(paste("Classification done for batch", index))
  }
  
  rm(temp)
  
  cls_pred
}