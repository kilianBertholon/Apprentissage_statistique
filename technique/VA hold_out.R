#Library caTools
VA_hold_out <- function(data, Ratio){
  library(caTools)
  
  set.seed(123)
  division <- sample.split(data, splitRatio = Ratio)
  train_data <- subset(data, division == TRUE)
  test_data <- subset(data, division == FALSE)
  
  return(list(train_data= train_data, test_data = test_data))
}



VA_leave_one_out <- function(data, Y, model){
  library(caret)
  
  train_control = trainControl(method = "LOOCV")
  model <- train(Y~., data, method = model, trControl = train_control)
  
  return(model)
}

VA_leave_p_out <- function(data, Y, model, nb_ite){
  library(caret)
  train_control = trainControl(method = "LGOCV")
  
  model <- train(
    Y ~ .,
    data = data,
    method = model,
    trControl = train_control(
      method = "cv",
      number = nb_ite,
      allowParallel = TRUE)
    )
  
  print(summary(model))
  return(model)
}



  
}