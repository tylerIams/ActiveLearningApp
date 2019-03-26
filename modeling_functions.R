
####This script is for active learning modeling
active_model <- NULL


####This function takes the active set as a parameter and
####creates two folds, it creates a ridge regression (glmnet)
####model with each fold, uses the opposite fold to predict
####with the model, and measures the model's accuracy.
###It logs the accuracy in a table (acc_tab) and returns it.
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
  active_model <<- glmnet(x_train, y_train, alpha=0.0, 
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


####This function takes the candidate set as a parameter and
####finds the probability of each featurized image belonging
####to each class, it then finds the points of least confidence,
####(e.g. the points with the lowest max probability), and returns
####the set arranged in ascending order by least confidence.
findDataToLabel <- function(candidate_set) {
  x_test <- model.matrix(~ ., select(candidate_set, -image, -label))
  x_test <- x_test[,-1]
  y_test <- candidate_set$label
  
  category_prob <- predict(active_model, newx=x_test, type="response")
  max_probs <- apply(category_prob, 1, FUN = max)
  candidate_set <- cbind(candidate_set, max_probs)
  candidate_set <- candidate_set %>% arrange(max_probs)
}



