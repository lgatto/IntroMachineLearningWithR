## ----setup, echo=FALSE, warning=FALSE------------------------------------
source("./src/setup.R")

## ----irisuml, echo=FALSE-------------------------------------------------
data(iris)
knitr::kable(head(iris[, 1:4]))

## ----irissml, echo=FALSE-------------------------------------------------
knitr::kable(head(iris[, c(5, 1:4)]))

## ------------------------------------------------------------------------
data(iris)

## ----dtiris--------------------------------------------------------------
datatable(iris)

## ------------------------------------------------------------------------
data(mtcars)

## ----dtmtcars, fig.cap=""------------------------------------------------
datatable(mtcars)

## ----prolocinstall, eval=FALSE-------------------------------------------
## source("http://www.bioconductor.org/biocLite.R")
## biocLite(c("MSnbsase", "pRoloc")) ## software
## biocLite("pRolocdata") ## date

## ------------------------------------------------------------------------
library("ggplot2")
data(diamonds)

## ----dtdiamonds, fig.cap=""----------------------------------------------
datatable(diamonds)

## ------------------------------------------------------------------------
library("mlbench")
data(Sonar)

## ----dtsonar, fig.cap=""-------------------------------------------------
datatable(Sonar)

## ---- message=FALSE------------------------------------------------------
library("MASS")
data(Boston)

## ----dtboston, fig.cap=""------------------------------------------------
datatable(Boston)

## ------------------------------------------------------------------------
library("C50")
data(churn)
dim(churnTrain)
dim(churnTest)

## ----dtchurn, fig.cap=""-------------------------------------------------
datatable(churnTrain)

## ---- eval=FALSE---------------------------------------------------------
## stats::kmeans(x, centers = 3, nstart = 10)

## ----solirisx, echo=FALSE------------------------------------------------
i <- grep("Length", names(iris))
x <- iris[, i]

## ----solkmcl, echo=FALSE-------------------------------------------------
cl <- kmeans(x, 3, nstart = 10)

## ----solkmplot, echo=FALSE, fig.cap = "k-means algorithm on sepal and petal lengths"----
plot(x, col = cl$cluster)

## ----soliris, eval=FALSE-------------------------------------------------
## i <- grep("Length", names(iris))
## x <- iris[, i]
## cl <- kmeans(x, 3, nstart = 10)
## plot(x, col = cl$cluster)

## ----kmworksinit, fig.cap="k-means random intialisation"-----------------
set.seed(12)
init <- sample(3, nrow(x), replace = TRUE)
plot(x, col = init)

## ----kmworksiter, fig.width = 12, fig.cap="k-means iteration: calculate centers (left) and assign new cluster membership (right)"----
par(mfrow = c(1, 2))
plot(x, col = init)
centres <- sapply(1:3, function(i) colMeans(x[init == i, ], ))
centres <- t(centres)
points(centres[, 1], centres[, 2], pch = 19, col = 1:3)

tmp <- dist(rbind(centres, x))
tmp <- as.matrix(tmp)[, 1:3]

ki <- apply(tmp, 1, which.min)
ki <- ki[-(1:3)]

plot(x, col = ki)
points(centres[, 1], centres[, 2], pch = 19, col = 1:3)

## ----selrep, fig.width = 12, fig.cap = "Different k-means results on the same (random) data"----
cl1 <- kmeans(x, centers = 3, nstart = 10)
cl2 <- kmeans(x, centers = 3, nstart = 10)
table(cl1$cluster, cl2$cluster)

cl1 <- kmeans(x, centers = 3, nstart = 1)
cl2 <- kmeans(x, centers = 3, nstart = 1)
table(cl1$cluster, cl2$cluster)

set.seed(42)
xr <- matrix(rnorm(prod(dim(x))), ncol = ncol(x))
cl1 <- kmeans(xr, centers = 3, nstart = 1)
cl2 <- kmeans(xr, centers = 3, nstart = 1)
table(cl1$cluster, cl2$cluster)
diffres <- cl1$cluster != cl2$cluster
par(mfrow = c(1, 2))
plot(xr, col = cl1$cluster, pch = ifelse(diffres, 19, 1))
plot(xr, col = cl2$cluster, pch = ifelse(diffres, 19, 1))

## ----kmelbow, echo=FALSE, fig.cap = ""-----------------------------------
ks <- 1:5
tot_within_ss <- sapply(ks, function(k) {
    cl <- kmeans(x, k, nstart = 10)
    cl$tot.withinss
})
plot(ks, tot_within_ss, type = "b",
     ylab = "Total within squared distances",
     xlab = "Values of k tested")

## ----solkmelbow----------------------------------------------------------
ks <- 1:5
tot_within_ss <- sapply(ks, function(k) {
    cl <- kmeans(x, k, nstart = 10)
    cl$tot.withinss
})
plot(ks, tot_within_ss, type = "b")

## ----hcldata, fig.width = 12, echo=FALSE, fig.cap = "Hierarchical clustering: initialisation (left) and colour-coded results after iteration (right)."----
set.seed(42)
xr <- data.frame(x = rnorm(5),
                 y = rnorm(5))
cls <- c("red", "blue", "orange", "blue", "orange")
cls <- scales::col2hcl(cls, alpha = 0.5)
par(mfrow = c(1, 2))
plot(xr, cex = 3)
text(xr$x, xr$y, 1:5)
plot(xr, cex = 3, col = cls, pch = 19)
text(xr$x, xr$y, 1:5)

## ----hcldendro, echo=FALSE, fig.cap = "Visualisation of the hierarchical clustering results on a dendrogram"----
plot(hcr <- hclust(dist(xr)))

## ----hclsol, fig.cap = ""------------------------------------------------
d <- dist(iris[, 1:4])
hcl <- hclust(d)
hcl
plot(hcl)

## ----cuthcl, echo=FALSE, fig.cap = "Cutting the dendrogram at height 1.5."----
plot(hcr)
abline(h = 1.5, col = "red")

## ----cuthclsol-----------------------------------------------------------
plot(hcl)
abline(h = 3.9, col = "red")
cutree(hcl, k = 3)
cutree(hcl, h = 3.9)
identical(cutree(hcl, k = 3), cutree(hcl, h = 3.9))

## ----iris2algs, fig.width = 12, fig.cap = ""-----------------------------
km <- kmeans(iris[, 1:4], centers = 3, nstart = 10)
hcl <- hclust(dist(iris[, 1:4]))
table(km$cluster, cutree(hcl, k = 3))
par(mfrow = c(1, 2))
plot(iris$Petal.Length, iris$Sepal.Length, col = km$cluster, main = "k-means")
plot(iris$Petal.Length, iris$Sepal.Length, col = cutree(hcl, k = 3), main = "Hierarchical clustering")
## Checking with the labels provided with the iris data
table(iris$Species, km$cluster)
table(iris$Species, cutree(hcl, k = 3))

## ----scalesol, fig.width=12, fig.cap=""----------------------------------
colMeans(mtcars)
hcl1 <- hclust(dist(mtcars))
hcl2 <- hclust(dist(scale(mtcars)))
par(mfrow = c(1, 2))
plot(hcl1, main = "original data")
plot(hcl2, main = "scaled data")

## ----pcaex, echo=FALSE, fig.width=12, fig.height=4, fig.cap="Original data (left). PC1 will maximise the variability while minimising the residuals (centre). PC2 is orthogonal to PC1 (right)."----
set.seed(1)
xy <- data.frame(x = (x <- rnorm(50, 2, 1)),
                 y = x + rnorm(50, 1, 0.5))
pca <- prcomp(xy)

z <- cbind(x = c(-1, 1), y = c(0, 0))
zhat <- z %*% t(pca$rotation[, 1:2])
zhat <- scale(zhat, center = colMeans(xy), scale = FALSE)
par(mfrow = c(1, 3))
plot(xy, main = "Orignal data (2 dimensions)")
plot(xy, main = "Orignal data with PC1")
abline(lm(y ~ x, data = data.frame(zhat - 10)), lty = "dashed")
grid()
plot(pca$x, main = "Data in PCA space")
grid()

## ----irispairs, fig.cap=""-----------------------------------------------
pairs(iris[, -5], col = iris[, 5], pch = 19)

## ----irispca-------------------------------------------------------------
irispca <- prcomp(iris[, -5])
summary(irispca)

## ----histpc1, echo=FALSE, fig.cap="Iris data along PC1."-----------------
## boxplot(irispca$x[, 1] ~ iris[, 5], ylab = "PC1")
hist(irispca$x[iris$Species == "setosa", 1],
     xlim = range(irispca$x[, 1]), col = "#FF000030",
     xlab = "PC1", main = "PC1 variance explained 92%")
rug(irispca$x[iris$Species == "setosa", 1], col = "red")
hist(irispca$x[iris$Species == "versicolor", 1], add = TRUE, col = "#00FF0030")
rug(irispca$x[iris$Species == "versicolor", 1], col = "green")
hist(irispca$x[iris$Species == "virginica", 1],  add = TRUE, col = "#0000FF30")
rug(irispca$x[iris$Species == "virginica", 1], col = "blue")

## ----irisbiplot, fig.cap=""----------------------------------------------
biplot(irispca)

## ----irispcavar----------------------------------------------------------
var <- irispca$sdev^2
(pve <- var/sum(var))
cumsum(pve)

## ----irispcax, echo=FALSE, fig.width=12, fig.cap=""----------------------
par(mfrow = c(1, 2))
plot(irispca$x[, 1:2], col = iris$Species)
plot(irispca$x[, 3:4], col = iris$Species)

## ----irispcaxcol, eval=FALSE---------------------------------------------
## par(mfrow = c(1, 2))
## plot(irispca$x[, 1:2], col = iris$Species)
## plot(irispca$x[, 3:4], col = iris$Species)

## ----scalepcasol, fig.with=12, fig.cap=""--------------------------------
par(mfrow = c(1, 2))
biplot(prcomp(mtcars, scale = FALSE), main = "No scaling")  ## 1
biplot(prcomp(mtcars, scale = TRUE), main = "With scaling") ## 2

## ----dummvar, echo=FALSE-------------------------------------------------
dfr <- data.frame(x = c(1, 0, 0),
                  y = c(0, 1, 0))
rownames(dfr) <- LETTERS[1:3]
knitr::kable(dfr)

## ----iristsne, fig.cap=""------------------------------------------------
library("Rtsne")
uiris <- unique(iris[, 1:5])
iristsne <- Rtsne(uiris[, 1:4])
plot(iristsne$Y, col = uiris$Species)

## ----knnex, echo=FALSE, fig.cap="Schematic illustrating the k nearest neighbors algorithm."----
p1 <- c(0, 0)
p2 <- c(0.7, 0.5)
x1 <- rbind(c(0.2, 0.2),
            c(-0.3, -0.8),
            c(-0.2, 1.3))
x2 <- rbind(c(1, 1),
            c(0.5, 0.7))
x3 <- c(1.5, -.9)
x <- rbind(p1, p2, x1, x2, x3)
col <- c("black", "black",
         rep("steelblue", 3),
         rep("red", 2),
         "darkgreen")

plot(x, pch = 19, col = col,
     cex = 5, xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
grid()
text(p1[1], p1[2], "1", col = "white", cex = 2)
text(p2[1], p2[2], "2", col = "white", cex = 2)
for (i in 1:3)
    segments(p1[1], p1[2],
             x1[i, 1], x1[i, 2],
             lty = "dotted",
             col = "steelblue")
segments(p2[1], p2[2],
         x1[1, 1], x1[1, 2],
         lty = "dotted",
         col = "steelblue")
for (i in 1:2)
    segments(p2[1], p2[2],
             x2[i, 1], x2[i, 2],
             lty = "dotted",
             col = "red")
legend("topright",
       legend = expression(c[1], c[2], c[3]),
       pch = 19,
       col = c("steelblue", "red", "darkgreen"),
       cex = 2,
       bty = "n")

## ----knn1----------------------------------------------------------------
set.seed(12L)
tr <- sample(150, 50)
nw <- sample(150, 50)
library("class")
knnres <- knn(iris[tr, -5], iris[nw, -5], iris$Species[tr])
head(knnres)

## ----knn1acc-------------------------------------------------------------
table(knnres, iris$Species[nw])
mean(knnres == iris$Species[nw])

## ----knnargs-------------------------------------------------------------
args(knn)

## ----knn5----------------------------------------------------------------
knnres5 <- knn(iris[tr, -5], iris[nw, -5], iris$Species[tr], k = 5)
mean(knnres5 == iris$Species[nw])
table(knnres5, knnres)

## ----knn5prob------------------------------------------------------------
knnres5prob <- knn(iris[tr, -5], iris[nw, -5], iris$Species[tr], k = 5, prob = TRUE)
table(attr(knnres5prob, "prob"))

## ----caretlib------------------------------------------------------------
library("caret")

## ------------------------------------------------------------------------
data(diamonds)
model <- lm(price ~ ., diamonds)
p <- predict(model, diamonds)

## ------------------------------------------------------------------------
## Error on prediction
error <- p - diamonds$price
rmse_in <- sqrt(mean(error^2)) ## in-sample RMSE 
rmse_in

## ------------------------------------------------------------------------
set.seed(42)
ntest <- nrow(diamonds) * 0.80
test <- sample(nrow(diamonds), ntest)
model <- lm(price ~ ., data = diamonds[test, ])
p <- predict(model, diamonds[-test, ])
error <- p - diamonds$price[-test]
rmse_out <- sqrt(mean(error^2)) ## out-of-sample RMSE 
rmse_out

## ----trxval--------------------------------------------------------------
set.seed(42)
model <- train(price ~ ., diamonds,
               method = "lm", 
               trControl = trainControl(method = "cv", 
                                        number = 10, 
                                        verboseIter = FALSE))
model

## ----texval--------------------------------------------------------------
p <- predict(model, diamonds)
error <- p - diamonds$price
rmse_xval <- sqrt(mean(error^2)) ## xval RMSE
rmse_xval

## ----xvalsol-------------------------------------------------------------
library("MASS")
data(Boston)
model <- train(medv ~ .,
               Boston,
               method = "lm", 
               trControl = trainControl(method = "cv", 
                                        number = 10))
model
p <- predict(model, Boston)
sqrt(mean(p - Boston$medv)^2)

## ----cmat, echo=FALSE----------------------------------------------------
cmat <- data.frame(c("TP", "FN"),
                   c("FP", "TN"))
rownames(cmat) <- c("Predicted Yes", "Predicted No")
colnames(cmat) <- c("Reference Yes", "Reference No")
knitr::kable(cmat)

## ----sonarex0------------------------------------------------------------
library("mlbench")
data(Sonar)
## 60/40 split
tr <- sample(nrow(Sonar), round(nrow(Sonar) * 0.6))
train <- Sonar[tr, ]
test <- Sonar[-tr, ]

## ----sonarex1, warning = FALSE-------------------------------------------
model <- glm(Class ~ ., data = train, family = "binomial")
p <- predict(model, test, type = "response")
summary(p)

## ----sonarex2------------------------------------------------------------
cl <- ifelse(p > 0.5, "M", "R")
table(cl, test$Class)

## ----soncmat-------------------------------------------------------------
confusionMatrix(cl, test$Class)

## ----confmatsol----------------------------------------------------------
confusionMatrix(ifelse(p > 0.9, "M", "R"), test$Class)
confusionMatrix(ifelse(p > 0.1, "M", "R"), test$Class)

## ------------------------------------------------------------------------
caTools::colAUC(p, test[["Class"]], plotROC = TRUE)

## ----trctrlroc, warning = FALSE------------------------------------------
## Create trainControl object: myControl
myControl <- trainControl(
    method = "cv", ## cross validation
    number = 10,   ## 10-fold
    summaryFunction = twoClassSummary, ## NEW
    classProbs = TRUE, # IMPORTANT
    verboseIter = FALSE
)
## Train glm with custom trainControl: model
model <- train(Class ~ ., Sonar,
               method = "glm", ## to use glm's logistic regression
               trControl = myControl) 

## Print model to console
print(model)

## ----rpart, fig.cap="Descision tree with its if-else conditions"---------
library("rpart") ## recursive partitioning
m <- rpart(Class ~ ., data = Sonar,
           method = "class")
library("rpart.plot")
rpart.plot(m)
p <- predict(m, Sonar, type = "class")
table(p, Sonar$Class)

## ----loadrange, echo=FALSE, message=FALSE--------------------------------
suppressPackageStartupMessages(library("ranger"))

## ----rftrain, fig.cap="", cache=TRUE-------------------------------------
set.seed(12)
model <- train(Class ~ .,
               data = Sonar,
               method = "ranger") 
print(model)

## ----rfplotmodel, fig.cap=""---------------------------------------------
plot(model)

## ----tuneLength, eval = FALSE--------------------------------------------
## model <- train(Class ~ .,
##                data = Sonar,
##                method = "ranger",
##                tuneLength = 5)

## ----tuneGrid, fig.cap="", cache=TRUE------------------------------------
set.seed(42)
myGrid <- expand.grid(mtry = c(5, 10, 20, 40, 60),
                     splitrule = c("gini", "extratrees"))
model <- train(Class ~ .,
               data = Sonar,
               method = "ranger", 
               tuneGrid = myGrid,
               trControl = trainControl(method = "cv",
                                       number = 5,
                                       verboseIter = FALSE))
print(model)
plot(model)

## ----rftrainsol, cache=TRUE, fig.cap=""----------------------------------
set.seed(42)
model <- train(Class ~ .,
               data = Sonar,
               method = "ranger",
               tuneLength = 5,
               trControl = trainControl(method = "cv",
                                        number = 5,
                                        verboseIter = FALSE))
plot(model)

## ----makena--------------------------------------------------------------
data(mtcars)
mtcars[sample(nrow(mtcars), 10), "hp"] <- NA
Y <- mtcars$mpg    ## target variable
X <- mtcars[, 2:4] ## predictors

## ----rflib, echo=FALSE---------------------------------------------------
suppressPackageStartupMessages(library("randomForest"))


## ----trainna, warning=FALSE----------------------------------------------
try(train(X, Y))

## ---- eval=TRUE----------------------------------------------------------
train(X, Y, preProcess = "medianImpute")

## ---- eval=FALSE---------------------------------------------------------
## train(X, Y, preProcess = "knnImpute")

## ---- eval=FALSE---------------------------------------------------------
## train(X, Y, preProcess = "scale")
## train(X, Y, preProcess = "center")

## ---- eval=FALSE---------------------------------------------------------
## train(X, Y, preProcess = "pca")

## ---- eval=TRUE, warning=FALSE-------------------------------------------
train(X, Y, preProcess = c("knnImpute", "center", "scale", "pca"))

## ----churndata-----------------------------------------------------------
library("C50")
data(churn)
table(churnTrain$churn)/nrow(churnTrain)

## ----createFolds---------------------------------------------------------
myFolds <- createFolds(churnTrain$churn, k = 5)
str(myFolds)

## ----foldprop------------------------------------------------------------
sapply(myFolds, function(i) {
    table(churnTrain$churn[i])/length(i)
})

## ----trctrol-------------------------------------------------------------
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProb = TRUE,
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds
)

## ----glmnetmodel, fig.cap=""---------------------------------------------
glm_model <- train(churn ~ .,
                   churnTrain,
                   metric = "ROC", 
                   method = "glmnet",
                   tuneGrid = expand.grid(
                       alpha = 0:1,
                       lambda = 0:10/10),
                   trControl = myControl)
print(glm_model)
plot(glm_model)

## ----rfmodel, cache=TRUE, fig.cap=""-------------------------------------
rf_model <- train(churn ~ .,
                  churnTrain,
                  metric = "ROC", 
                  method = "ranger",
                  tuneGrid = expand.grid(
                      mtry = c(2, 5, 10, 19),
                      splitrule = c("gini", "extratrees")),
                  trControl = myControl)
print(rf_model)
plot(rf_model)

## ----knnmodel, cache=TRUE, fig.cap=""------------------------------------
knn_model <- train(churn ~ .,
                   churnTrain,
                   metric = "ROC", 
                   method = "knn",
                   tuneLength = 20,
                   trControl = myControl)
print(knn_model)
plot(knn_model)

## ----svmmodel, cache=TRUE, fig.cap=""------------------------------------
svm_model <- train(churn ~ .,
                   churnTrain,
                   metric = "ROC", 
                   method = "svmRadial",
                   tuneLength = 10,
                   trControl = myControl)
print(svm_model)
plot(svm_model)

## ----nbmodel, fig.cap=""-------------------------------------------------
nb_model <- train(churn ~ .,
                  churnTrain,
                  metric = "ROC", 
                  method = "naive_bayes",
                  trControl = myControl)

print(nb_model)
plot(nb_model)

## ----resamples-----------------------------------------------------------
model_list <- list(glmmet = glm_model,
                   rf = rf_model,
                   knn = knn_model,
                   svm = svm_model,
                   nb = nb_model)
resamp <- resamples(model_list)
resamp
summary(resamp)

## ----plotresam, fig.cap = "Comparing distributions of AUC values for various models."----
lattice::bwplot(resamp, metric = "ROC")

## ----svmmodel2, cache=TRUE, fig.cap=""-----------------------------------
svm_model1 <- train(churn ~ .,
                    churnTrain,
                    metric = "ROC", 
                    method = "svmRadial",
                    tuneLength = 10,
                    trControl = myControl)

svm_model2 <- train(churn ~ .,
                    churnTrain[, c(2, 6:20)],
                    metric = "ROC", 
                    method = "svmRadial",
                    preProcess = c("scale", "center", "pca"),
                    tuneLength = 10,
                    trControl = myControl)

model_list <- list(svm1 = svm_model1,
                   svm2 = svm_model2)
resamp <- resamples(model_list)
summary(resamp)
bwplot(resamp, metric = "ROC")

## ------------------------------------------------------------------------
p <- predict(rf_model, churnTest)
confusionMatrix(p, churnTest$churn)

## ----si------------------------------------------------------------------
sessionInfo()

