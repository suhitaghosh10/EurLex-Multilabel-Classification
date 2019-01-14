# The script file contains utility methods used for classification

# The method performs classification based on the input params
# @input:
# mldrfilename -> name of the datset (arff or xml - both need to have same names)
# mode -> the multilabel transform to be used. the default value is br (binary transform)
# classifier -> name of the classifier used.
# reduceLabels -> flag specifying whether to perform classification over reduced labels
# @returns: average value of metrics-accuracy,hamming-loss,macro-AUC,macro-F1,macro-precision,macro-recall,micro-AUC,micro-F1,micro-precision,micro-recall,precision,recall,subset-accuracy
classify <- function(mldrfilename,
                     mode,
                     classifier,
                     reduceLabels) {
  #ratio of samples to be used for training
  train_ratio <- 0.65
  #ratio of samples to be used for testing
  test_ratio <- 0.35
  
  #peforms classification over batches of dataset.
  for (index in 1:batch_number) {
    #load dataset using mldr()
    if(!file.exists(paste(mldrfilename, index,".arff", sep = "")) || !file.exists(paste(mldrfilename, index,".xml", sep = "")))
      stop(paste("mldr files (arff/XML) ",mldrfilename,index," not found!", sep=""))
    
    ds <- mldr(paste(mldrfilename, index, sep = ""))
    if (reduceLabels == TRUE) {
      # disable the majority labels on instances with highly imbalanced labels
      ds <- ds[.SCUMBLE <= ds$measures$scumble]
    }
    
    ds <-  ds %>%
      #remove attribute having name "...", causes issue for random forest classifier
      remove_attributes("...") %>%
      #remove attributes having same value for all examples
      remove_unique_attributes() %>%
      #remove examples having no labels
      remove_unlabeled_instances() %>%
      #create holdout partition for training and testing using 'random' method
      create_holdout_partition(c(train = train_ratio, test = test_ratio))
    
    if (tolower(mode) == "lp")
      # Label powerset (LP) transform
      cl_model <- lp(ds$train, classifier)
    else if (tolower(mode) == "cc")
      # Classifier Chain (CC) transform
      cl_model <- cc(ds$train, classifier)
    else
      cl_model <-
      br(ds$train, classifier) # Binary Relevance (BR) transform
    
    prediction <- tryCatch(
      predict(cl_model, ds$test),
      error = function(e) {
        message(paste(
          "prediction could not be done for ",
          paste(mldrfilename, index, sep = "")
        ))
        NA
      }
    )
    
    #prediction for the current batch
    temp_prediction <-
      tryCatch(
        multilabel_evaluate(ds$test, prediction, "bipartition"),
        error = function(e) {
          message(paste(
            "evaluation could not be done for ",
            paste(mldrfilename, index, sep = "")
          ))
          NA
        }
      )
    
    if (index == 1 && !is.na(temp_prediction)) {
      if (exists('cls_pred'))
        rm(cls_pred)
      
      cls_pred <- temp_prediction
      
    } else if (index > 1 && !is.na(temp_prediction)) {
      if (!exists('cls_pred'))
        cls_pred <- temp_prediction
      else
        cls_pred <- cbind(cls_pred, temp_prediction)
    }
  }
  
  # get mean over all rows (metrics)
  if(batch_number>1){
    prediction_mean <- rowMeans(cls_pred, na.rm = TRUE)
    prediction_mean[is.na(prediction_mean)] <- 0
  } else { prediction_mean <- cls_pred}
  
  print(paste("Classification done for: ",mode,"-", classifier,sep = ""))
  prediction_mean
}