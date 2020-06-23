library(datasets)
library(skimr)
library(Seurat)
library (caret)
library(ggplot2)
library(lattice)
library(kernlab)
library(e1071)
data("iris")


#head/tail of data

head(iris,4)
tail(iris, 4)

#checking a summary of data

summary(iris)

#checking total missing data

sum(is.na(iris))

#larger summary

skim(iris)

#see summary for group of species rather than dependent variables

iris %>%
  
  dplyr::group_by(Species) %>%
  skim()

#Panel plots
plot(iris, col = "purple")

#Scatterplot

plot(iris$Sepal.Width, iris$Sepal.Length, col = "purple", xlab = "Sepal width", ylab =  "Sepal length")

#Histogram

hist(iris$Sepal.Width, col = "Purple")

#Plot with features

featurePlot(x = iris[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

#Classification model

set.seed(100)

TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

plot(TrainingSet, TestingSet)

#Build training model

Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model, training leaving out one group of data and predicting it,
#this is done to every k group od data, in this example they use 10 folds,
#so divide 120/10

Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)


# Apply model for prediction

Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)

Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance

Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")



