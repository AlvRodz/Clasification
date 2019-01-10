

# Package
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("MASS")) install.packages("MASS")
if (!require("knitr")) install.packages("knitr")
if (!require("ggcorrplot")) install.packages("ggcorrplot")
if (!require("pdp")) install.packages("pdp")
if (!require("mvShapiroTest")) install.packages("mvShapiroTest")
if (!require("biotools")) install.packages("biotools")
if (!require("heplots")) install.packages("heplots")
if (!require("car")) install.packages("car")
if (!require("klaR")) install.packages("klaR")
if (!require("haven")) install.packages("haven")
if (!require("caret")) install.packages("caret")

library(ggplot2)
library(MASS)
library(knitr)
library(ggcorrplot)
library(pdp)
library(mvShapiroTest)
library(biotools)
library(heplots)
library(car)
library(klaR)
library(caret)

data(iris)
iris.5 <- iris[1:5,]
kable(iris.5)



# EDA
iris.ver <- subset(iris, Species == "versicolor")
iris.set <- subset(iris, Species == "setosa")
iris.vir <- subset(iris, Species == "virginica")
par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(iris.ver[,1:4], main="Versicolor",ylim = c(0,8),las=2)
boxplot(iris.set[,1:4], main="Setosa",ylim = c(0,8),las=2)
boxplot(iris.vir[,1:4], main="Virginica",ylim = c(0,8),las=2)




# calculate correlations using cor and store the results
corr.iris <- cor(iris[,-5])

ggcorrplot(corr.iris,type = "upper",
           outline.col = "white", title = "Matriz de correlaciones lineales - atributos Iris",
           lab = TRUE)



# Pruebas de normalidad

## Test de normalidad shapiro-Wilks para distribuciones multivariantes.
### H0: La población esta normalmente distribuida.

### 1) Virginica
iris.virginica <- as.matrix(iris[iris$Species == "virginica",1:4],ncol=4) 

mvShapiro.Test(iris.virginica)

### se distribuye como una normal multivariante

### 2) setosa

iris.setosa <- as.matrix(iris[iris$Species == "setosa",1:4],ncol=4) 

mvShapiro.Test(iris.setosa)

### Se rechaza la hipótesis nula, no se distribuye como una normal multivariante.

### 3) versicolor

iris.versicolor <- as.matrix(iris[iris$Species == "versicolor",1:4],ncol=4) 

mvShapiro.Test(iris.versicolor)

### se distribuye como una normal multivariante


# Test de Homocedasticidad


# F-distribution
var.test(x = iris[iris$Species == "versicolor", "Petal.Length"],
         y = iris[iris$Species == "virginica", "Petal.Length"] )

## BoxM

## Test paramétrico que compara la varianza en muestras multivariantes. Comprueba si dos o más matrices de covarianzas son iguales (homogéneas)
## Ho: La matriz de covarianzas de las variables dependientes son iguales en todos los grupos/poblaciones
## H1: La matriz de covarianzas no es igual en todos los grupos

boxM(iris[, -5], iris[, 5])

### res

### summary(res)

## Barlett

## Ho: Todas las varianzas de una población k son iguales
## H1: La varianza de al menos dos poblaciones es diferente
# La prueba de Bartlett es sensible a las desviaciones de la normalidad. Es decir, si las muestras provienen de distribuciones 
# no normales, entonces la prueba de Bartlett puede ser simplemente para probar la no normalidad

bartlett.test(iris[,1]~Species, iris) # Sepal.length
bartlett.test(iris[,2]~Species, iris) # Sepal.Width
bartlett.test(iris[,3]~Species, iris) # Petal.Length
bartlett.test(iris[,4]~Species, iris) # Petal.Width

## Barlett todas

x <- data.frame(bartlettTests(iris[,1:4], iris$Species)) # Todas



## Levenne 

##Prueba estadística inferencial utilizada para evaluar la igualdad de las varianzas para una variable calculada para dos o más grupos.
#Ho: las varianzas poblacionales son iguales
#H1: hay una diferencia entre las variaciones en la población.

leveneTest(y = iris$Petal.Length, group = iris$Species, center = "median")
leveneTest(y = iris$Petal.Width, group = iris$Species, center = "median")
leveneTest(y = iris$Sepal.Length, group = iris$Species, center = "median")
leveneTest(y = iris$Sepal.Width, group = iris$Species, center = "median")

leveneTest(y = iris$Petal.Length, group = iris$Species, center = "mean")
leveneTest(y = iris$Petal.Width, group = iris$Species, center = "mean")
leveneTest(y = iris$Sepal.Length, group = iris$Species, center = "mean")
leveneTest(y = iris$Sepal.Width, group = iris$Species, center = "mean")

# Test de Brown-Forsyth


library(HH)
hov(iris$Sepal.Width ~ iris$Species)



# Si se tiene seguridad de que las muestras a comparar proceden de poblaciones que siguen una distribución normal, 
# son recomendables el F-test y el test de Bartlet, pareciendo ser el segundo más recomendable ya que el primero es muy potente
# pero extremadamente sensible a desviaciones de la normal. Si no se tiene la seguridad de que las poblaciones de origen son normales, 
# se recomiendan el test de Leven utilizando la mediana o el test no paramétrico Fligner-Killeen que también se basa en la mediana. 


## Visualization

x <- iris[,1:4]
y <- iris[,5]

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Cross validation

control <- trainControl(method="cv", number=10, p= 0.70)
metric <- "Accuracy"

# Analysis

set.seed(12345)
fit.lda <- train(Species~., data=iris, method="lda", metric=metric, trControl=control)
fit.lda

fit.lda$finalModel


set.seed(12345)
fit.qda <- train(Species~., data=iris, method="qda", metric=metric, trControl=control)
fit.qda


fit.qda$finalModel

# select model

results <- resamples(list(lda=fit.lda, qda=fit.qda))
summary(results)

# Nos quedamos con los dos

## result viz

dotplot(results)




# Predict

training_sample <- sample(c(T,F),nrow(iris),replace = T,prob = c(0.65,0.35))
train <- iris[training_sample,]
test <- iris[!training_sample,]

# LDA

## re-train the model
set.seed(12345)
fit.LDA <- lda( Species ~ ., train)
fit.LDA

##  testing

fit.LDA.C = predict(fit.LDA,test)
fit.LDA.C

table(test[,5],fit.LDA.C$class)

confusionMatrix(fit.LDA.C$class,test[,5])

## Viz

plot(fit.LDA, dimen = 1, type = "b")

# QDA 


## re-train the model
set.seed(12345)
fit.QDA <- qda( Species ~ ., train)
fit.QDA

##  testing

fit.QDA.C = predict(fit.QDA,test)
fit.QDA.C

table(test[,5],fit.QDA.C$class)

confusionMatrix(fit.QDA.C$class,test[,5])

## viz



