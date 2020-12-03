# UTS DMKM

# load data
library(readr)
wine <- read_csv("C:/Users/ASUS/Downloads/wine.data", 
                 col_names = FALSE)
View(wine)

# memberi nama kolom pada data
colnames(wine) = c("tipe", "alcohol", "acid", "ash", "alcalinity", 
                   "magnesium", "phenols", "flavanoids", "nonflavanoids", 
                   "proanth", "intansity", "hue", "wires", "proline")
head(wine)
wine <- data.frame(wine)
str(wine)

#Split Data
set.seed(1234)
sampel <- sample(2,nrow(wine),replace = T, prob = c(0.8,0.2))
train <- wine[sampel==1, ]
colnames(train) = c("tipe", "alcohol", "acid", "ash", "alcalinity", 
                   "magnesium", "phenols", "flavanoids", "nonflavanoids", 
                   "proanth", "intansity", "hue", "wires", "proline")
test <- wine[sampel==2, ]
colnames(test) = c("tipe", "alcohol", "acid", "ash", "alcalinity", 
                   "magnesium", "phenols", "flavanoids", "nonflavanoids", 
                   "proanth", "intansity", "hue", "wires", "proline")
print(paste("Jumlah train data :", nrow(train)))
print(paste("Jumlah test data :", nrow(test)))

## Decision Tree
library(party)
library(psych)
library(caret)

tree <- ctree(tipe~., data=train)
plot(tree)

## Classification Random FOrest
library(randomForest)

set.seed(1234)
model <- randomForest(tipe~., data=train)
model

### Confussion matrix
prediksiRF <- predict(model, test)
confusionMatrix(table(prediksiRF, test$tipe))

## SVM
library(tidyverse)
library(e1071)

qplot(tipe, alcohol, data=wine)


