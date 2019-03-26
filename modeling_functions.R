

createModels <- function(active_set) {
  
  #Divide the active set into folds
  fold1 <- sample(1:nrow(active_set), nrow(active_set)/2)
  fold2 <- active_set[-fold1,]
  fold1 <- active_set[fold1,]
  
  #Create the training set with fold1
  x_train <- model.matrix(~ ., select(fold1, -image, -label))
  x_train <- x_train[,-1]
  y_train <- fold1$label
  
  #Create the first model with fold1 as train and fold2 as test
  active_model <- glmnet(x_train, y_train, alpha=0.0, 
                         lambda=0.1,
                         family="multinomial")
  x_test <- model.matrix(~ ., select(fold2, -image, -label))
  x_test <- x_test[,-1]
  y_test <- fold2$label
  preds <- predict(active_model, newx = x_test, type="class")
  
  tab <- table(preds, y_test)
  sum <- 0
  acc <- -1.0
  if (nrow(tab) == ncol(tab)) {
    for (x in 1:nrow(tab)) {
      sum = sum + tab[x,x]
    }
    acc <- sum/sum(tab)
  }
  print(acc)
  acc_tab <- tibble(ROUND = 1, FOLD = 1, ACCURACY = acc)
  
  #Create the training set with fold2
  x_train <- model.matrix(~ ., select(fold2, -image, -label))
  x_train <- x_train[,-1]
  y_train <- fold2$label
  
  #Create the second model with fold2 as train and fold1 as test
  active_model <- glmnet(x_train, y_train, alpha=0.0, 
                         lambda=0.1,
                         family="multinomial")
  x_test <- model.matrix(~ ., select(fold1, -image, -label))
  x_test <- x_test[,-1]
  y_test <- fold1$label
  preds <- predict(active_model, newx = x_test, type="class")
  
  tab <- table(preds, y_test)
  sum <- 0
  acc <- -1.0
  if (nrow(tab) == ncol(tab)) {
    for (x in 1:nrow(tab)) {
      sum = sum + tab[x,x]
    }
    acc <- sum/sum(tab)
  }
  print(acc)
  acc_tab2 <- tibble(ROUND = 1, FOLD = 2, ACCURACY = acc)
  acc_tab <- rbind(acc_tab, acc_tab2)
  
  return(acc_tab)
}