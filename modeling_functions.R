
####This script is for active learning modeling
active_model <- NULL
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
createModels <- function(active_set, lambda, ROUND) {
  
  if (y == 0) {
    #Divide the active set into test and train
    train_ints <- sample(1:nrow(active_set), nrow(active_set)/2)
    train <<- active_set[train_ints,]
    test <<- active_set[-train_ints,]
  } else {
    train <<- active_set
  }
  
  y <<- y + 1
  
  #Create the training set with fold1
  x_train <- model.matrix(~ ., select(train, -image, -label))
  x_train <- x_train[,-1]
  y_train <- train$label
  
  print(str_c("label: ", train$label))
  
  #Create the first model with fold1 as train and fold2 as test
  active_model <<- glmnet(x_train, y_train, alpha=0.0, 
                         lambda=lambda,
                         family="multinomial")
  x_test <- model.matrix(~ ., select(test, -image, -label))
  x_test <- x_test[,-1]
  y_test <- test$label
  preds <- predict(active_model, newx = x_test, type="class")
  
  tab <- table(preds, y_test)
  sum <- 0
  acc <- -1.0
  if (nrow(tab) == ncol(tab)) {
    for (x in 1:nrow(tab)) {
      sum = sum + tab[x,x]
    }
    acc1 <<- sum/sum(tab)
  }
  print(acc1)
  
  acc_tab <- tibble(ROUND = ROUND, ACCURACY = acc1)
  
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
  return(candidate_set)
}



