library(microbenchmark)
## a class exmaple that have 2 courses
## using the tapply we calulate the mean grade for the class
## tapply basically group each class and then 
## calcuate the mean grade for each group
class.exmple <-
  data.frame(student = 1:100,
             grade = rnorm(100, mean = 60, sd = 12),
             class = gl(2, 50,labels = c("DataAnalysis", "OS")))

tapply(class.exmple$grade, class.exmple$class, mean)


tapplyUsingFunction <- function(dt) {
summationT = 0;
summationC = 0;
for (i in seq_along(dt$grade)) {
  if (as.character(dt$class[i])=="DataAnalysis") {
     summationT <- dt$grade[i]+summationT
     #print(as.character(class.exmple$class[i]))
  }
  if (dt$class[i]=="OS") {
    summationC <- dt$grade[i]+summationC
    }
}
#print(summationT/50);
#print(summationC/50);
}
tapplyUsingFunction(class.exmple)

microbenchmark(tapplyUsingFunction(class.exmple),tapply(class.exmple$grade, class.exmple$class, mean))


#mapply gives us a way to call a non-vectorized function in a vectorized way.

mapply(rep,1:4,4:1)

mapplyUsingFunction <- function(){
outputList={}
for (i in 1:4) {
  outputList<-append(rep(i,4-i+1),outputList)
}
outputList
}
time<-microbenchmark(mapplyUsingFunction(),mapply(rep,1:4,4:1))
