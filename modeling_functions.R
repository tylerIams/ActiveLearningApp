
####This script is for active learning modeling
model <- NULL
y <- 0
train <<- NULL
test <<- NULL
acc1 <<- .90


## Retrain with external cross validation, 
## select best lambda from about 20 diff models, use validation set to choose
## the best lambda value, then let the user label about 10 more images, then run it again.
##

## Demos --> Whatever classifier you think is interesting?  Fascinated by dermatology dataset?
## That might be example to use active learning b/c you could use those to train humans or some shit
## MNist?


####This function takes the active set as a parameter and
####creates two folds, it creates a ridge regression (glmnet)
####model with each fold, uses the opposite fold to predict
####with the model, and measures the model's accuracy.
###It logs the accuracy in a table (acc_tab) and returns it.
createModels <- function(train_set, lambda, validation_set, test_set, ROUND) {
  lambdas <- 10^(seq(2, -5, length.out=22))
  # if (y == 0) {
  #   #Divide the active set into test and train
  #   train_ints <- sample(1:nrow(train_set), nrow(train_set)/2)
  #   train <<- train_set[train_ints,]
  #   test <<- train_set[-train_ints,]
  #   
  # } else {
  #   train <<- train_set
  # }
  train <<- train_set
  test <<- test_set
  y <<- y + 1
  
  #Create the training set with fold1
  x_train <- model.matrix(~ ., select(train, -image, -label))
  x_train <- x_train[,-1]
  y_train <- train$label
  
  print(str_c("label: ", train$label))
  
  model <<- glmnet(x_train, y_train, alpha=0.0,
                  lambda=lambdas,
                  family="binomial")
  val_set<- model.matrix(~ ., select(validation_set, -image, -label))
  X_val <- val_set[,-1]
  y_val <- validation_set$label
  
  validation_score_matrix <- predict(model, X_val, type="response")
  auc_vector <- apply(validation_score_matrix, 
                      2, 
                      function(score_vector){
                        pROC::auc(factor(y_val), score_vector, direction='<') %>% 
                          as.numeric
                      })
  best_lambda_idx <- which.max(auc_vector)
  validation_auc <- auc_vector[best_lambda_idx]
  lambda <<- model$lambda[best_lambda_idx]
  print(auc_vector)
  print ("best lambda: ", str_c(lambda))
  
  #Create the first model with fold1 as train and fold2 as test
  # active_model <<- glmnet(x_train, y_train, alpha=0.0, 
  #                        lambda=lambda,
  #                        family="multinomial")
  x_test <- model.matrix(~ ., select(test, -image, -label))
  x_test <- x_test[,-1]
  y_test <- test$label
  preds <- predict(model, newx = x_test, s=lambda, type="response")[,1]
  test_set_auc <- pROC::auc(factor(y_test), preds, direction='<') %>% as.numeric
  print("test_set_auc:   ", test_set_auc)
  # tab <- table(preds, y_test)
  # sum <- 0
  # acc <- -1.0
  # if (nrow(tab) == ncol(tab)) {
  #   for (x in 1:nrow(tab)) {
  #     sum = sum + tab[x,x]
  #   }
  #   acc1 <<- sum/sum(tab)
  # }
  # print(acc1)
  
  acc_tab <- tibble(ROUND = ROUND, AUC_ACCURACY = test_set_auc)
  
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
  
  category_prob <- predict(model, newx=x_test, s=lambda, type="response")
  print(category_prob)
  max_probs <- apply(category_prob, 1, FUN = max)
  candidate_set <- cbind(candidate_set, max_probs)
  candidate_set$def <- 0
  candidate_set['def'] <- abs(candidate_set$max_probs-0.5)
  candidate_set <- candidate_set %>% arrange(def)
  print(candidate_set$max_probs)
  candidate_set$def<- NULL
  return(candidate_set)
}



