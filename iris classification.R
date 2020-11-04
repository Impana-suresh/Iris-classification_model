#method1
library(datasets)
data("iris")
iris <- datasets::iris

#method 2
library(RCurl)
iris3 <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv") )
View(iris)  #viewing data with capital V

#displaying statistics
head(iris, 5)
tail(iris, 5)

# summary()
#summary of the entire one column
summary(iris)#entire data set
summary(iris$Petal.Length)

# Check to see if there are missing data?
#syn: sum(is.na(dataset_name))
#0--no na values
sum(is.na(iris))

# skimr() - expands on summary() by providing larger set of statistics
#  install.packages("skimr")
#skim() works on grouped data
#gives a brief summary on the data set. Gives the mean, sd, p0, and rough histogram of the data set 
# one comand
install.packages("skimr")
library(skimr)

skim(iris)

# Group data by Species then perform skim
iris %>% 
  dplyr::group_by(Species) %>% 
  skim() 

#data visulaization using Plot()

#plot type -- panel plots
plot(iris)#5 plots for 5 variables.
plot(iris, col = "blue")#colour changing

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red",xlab = "Sepal width", ylab = "Sepal length")
#for specific variable scatter pllot
#syn-- plot(dataset$var_name1, dataset$var_name2)1-col 2-row
#xlab and ylab to give a specific NAME to the axis.

#Histogram-- range in value distribution
hist(iris$Sepal.Width, col = "purple")

#feature plot

#necessary packages to run feature plot
install.packages("caret")
intsall.packages("ISLR")
intsall.packages("ggplot2")

#allows us to create skewer plot of eah of the 4 variables and inisde 
#it as box plot of each of the variables
library(ISLR)
library(caret)
library(ggplot2)
featurePlot(x = iris[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
#inference from the Feature plot
#SESTOSA-- lower in petal length and width ,,higher in sepal width 
#VERSICOLOR-- admist in all
#VIRGINICA-- higher in petal.length and width,, second higher in sepal.width

#CLASSIFICATION  MODEL
#assigning fixed seed number to variables
# To achieve reproducible model; set the random seed number
set.seed(100)

#data split
#2 sets-- training and testing dataset
# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

#SVM model (polynomial kernel)

Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)
#na.action--na.omit







