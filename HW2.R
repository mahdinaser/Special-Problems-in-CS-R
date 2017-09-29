library(microbenchmark)
## Craete the data frame for courses and grades
class.exmple <-
  data.frame(student = 1:100,
             grade = rnorm(100, mean = 60, sd = 12),
             class = gl(2, 50,labels = c("DataAnalysis", "OS")))


## Run the tapply to calculate the mean for each course

tapply(class.exmple$grade, class.exmple$class, mean)


## Create a function to simulate the tapply

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
print(summationT/50);
print(summationC/50);
}
tapplyUsingFunction(class.exmple)

# Compare using the microbenchmark 
momTotal = microbenchmark(tapplyUsingFunction(class.exmple),tapply(class.exmple$grade, class.exmple$class, mean))
autoplot(momTotal)


mapply(rep,1:4,4:1)

mapplyUsingFunction <- function(n){
outputList={}
for (i in 1:n) {
  outputList<-append(rep(i,n-i+1),outputList)
}
outputList
}

momTotal = microbenchmark(mapplyUsingFunction(bench),mapply(rep,1:bench,bench:1))
autoplot(momTotal)
