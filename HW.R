
library(gmodels)
library(class)

data("iris")
# function to create NA(s) in the data
insert_nas <- function(x,p) {
  if(is.numeric(x)) {

  rng<-max(x)-min(x)
  if (rng>5)
    {p<-p/4}
  else if (rng>3)
    {p<-p/3}
  else
  {p<-p/1.5}

  len <- length(x)
  n <- sample(1:floor(p*len), 1)
  i <- sample(1:len, n)
  x[i] <- NA
  }
  x

}

# Create the function to calculate Mode
getmode <- function(v) {
  v = na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the RMSE
rmse <- function(error){
  sqrt(mean(error^2))
}

calculateRSMEForImputedData<-function(probability){
originalData<-iris
dataWithNA <- as.data.frame(sapply(originalData[1:5],insert_nas,p=probability))

#uncomment each time replace the imputed method
dataRemovedNAs<-sapply(dataWithNA, function(x) {if(is.numeric(x)) ifelse(is.na(x),getmode(x),x) else x}) # for mode
#dataRemovedNAs<-sapply(dataWithNA, function(x) {if(is.numeric(x)) ifelse(is.na(x),mean(x,na.rm=T),x) else x})
#dataRemovedNAs<-sapply(dataWithNA, function(x) {if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x})
rmseError <- originalData[,1:4] - dataRemovedNAs[,1:4]


print(probability*100)
print(rmse(rmseError))



## This part of the code is to RUN KNN
ind <- sample(2, nrow(dataRemovedNAs), replace=TRUE, prob=c(0.7, 0.3))

trainData <- dataRemovedNAs[ind==1,-5]
testData <- dataRemovedNAs[ind==2,-5]

iris_train_labels <- dataRemovedNAs[ind==1,5]
iris_test_labels <- dataRemovedNAs[ind==2,5]

iris_test_pred1 <- knn(train = trainData, test = testData , cl= iris_train_labels,k = 3,prob=TRUE)
CrossTable(x = iris_test_labels, y = iris_test_pred1,prop.chisq=FALSE)

}


# Run the function with different paramters
calculateRSMEForImputedData(0.02)
calculateRSMEForImputedData(0.05)
calculateRSMEForImputedData(0.1)
calculateRSMEForImputedData(0.15)
calculateRSMEForImputedData(0.2)
calculateRSMEForImputedData(0.25)


