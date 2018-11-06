Classification of Iris Flowers
================

/// Credit to machinelearningmastery.com/machine-learning-in-r-step-by-step/ ///

As an introduction to machine learning, I will construct a multi-variable regression model with the intent of classifying a dataset of iris flowers. This dataset contains 150 observations of flowers, and 5 characteristics associated with each - 4 numeric measurements, and the species.

``` r
#load the caret package (http://topepo.github.io/caret/index.html)
library(lattice)
library(ggplot2)
library(caret)

# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris
```

I will need a dataset for validation of the model, and so I will partition the entire dataset into two parts, 80% for training and 20% for validation.

``` r
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
```

Let's explore the data we have.

``` r
# dimensions of dataset
dim(dataset)
```

    ## [1] 120   5

``` r
# list types for each attribute
sapply(dataset, class)
```

    ## Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
    ##    "numeric"    "numeric"    "numeric"    "numeric"     "factor"

``` r
# take a peek at the first 5 rows of the data
head(dataset)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 8          5.0         3.4          1.5         0.2  setosa
    ## 9          4.4         2.9          1.4         0.2  setosa

``` r
# list the levels for the class - i.e, the iris species
levels(dataset$Species)
```

    ## [1] "setosa"     "versicolor" "virginica"

``` r
# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)
```

    ##            freq percentage
    ## setosa       40   33.33333
    ## versicolor   40   33.33333
    ## virginica    40   33.33333

``` r
# summarize attribute distributions
summary(dataset)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.200   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.175   1st Qu.:2.800   1st Qu.:1.500   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.250   Median :1.300  
    ##  Mean   :5.840   Mean   :3.078   Mean   :3.763   Mean   :1.207  
    ##  3rd Qu.:6.400   3rd Qu.:3.400   3rd Qu.:5.100   3rd Qu.:1.825  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :40  
    ##  versicolor:40  
    ##  virginica :40  
    ##                 
    ##                 
    ## 

Now, let's visualise the data and get a better look at it.

I'll start with univariate plots, to better understand the properties of each variable.

``` r
# split input and output - the measurements define x, and the classes, y
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
  for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}
```

![](Iris_Species_ML_files/figure-markdown_github/visualise%201-1.png)

``` r
# barplot for class breakdown
plot(y)
```

![](Iris_Species_ML_files/figure-markdown_github/visualise%201-2.png) This tells me about the measurement distributions, and that the observations are equal in number.

///

Multivariate plots can tell me about the relationships between the different variables.

``` r
# scatterplot matrix - this creates a chart of each variable plotted against every other, allowing trend recognition
featurePlot(x=x, y=y, plot="ellipse")
```

![](Iris_Species_ML_files/figure-markdown_github/visualise%202-1.png)

``` r
# box and whisker plots for each attribute - as in the above boxplots, this allows distribution comparison, but also can identify clear differences between classes
featurePlot(x=x, y=y, plot="box")
```

![](Iris_Species_ML_files/figure-markdown_github/visualise%202-2.png)

``` r
# density plots for each attribute by class value - from this, I can see the distribution of each attribute in a mooth manner
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
```

![](Iris_Species_ML_files/figure-markdown_github/visualise%202-3.png)

``` r
#These look somewhat Gaussian!
```

Now that I have a deeper understanding of the data properties, it's time to set up some ML algorithms for predicting the class of each flower from its attributes.

I'll split the dataset into 10: 9 for training, 1 for for testing. I will repeat each process 3 times, for accuracy.

``` r
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
```

I'll use the 5 suggested algorithms for my dataset: LDA, CART, kNN, SVM, RF

``` r
# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)
```

And then compare them in terms of accuracy

``` r
#summarise accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
```

    ## 
    ## Call:
    ## summary.resamples(object = results)
    ## 
    ## Models: lda, cart, knn, svm, rf 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu. Max. NA's
    ## lda  0.9166667 1.0000000 1.0000000 0.9833333 1.0000000    1    0
    ## cart 0.8333333 0.9166667 0.9166667 0.9250000 0.9791667    1    0
    ## knn  0.9166667 0.9166667 0.9583333 0.9583333 1.0000000    1    0
    ## svm  0.9166667 0.9166667 0.9583333 0.9583333 1.0000000    1    0
    ## rf   0.8333333 0.9166667 0.9583333 0.9500000 1.0000000    1    0
    ## 
    ## Kappa 
    ##       Min. 1st Qu. Median   Mean 3rd Qu. Max. NA's
    ## lda  0.875   1.000 1.0000 0.9750 1.00000    1    0
    ## cart 0.750   0.875 0.8750 0.8875 0.96875    1    0
    ## knn  0.875   0.875 0.9375 0.9375 1.00000    1    0
    ## svm  0.875   0.875 0.9375 0.9375 1.00000    1    0
    ## rf   0.750   0.875 0.9375 0.9250 1.00000    1    0

``` r
# compare accuracy of models
dotplot(results)
```

![](Iris_Species_ML_files/figure-markdown_github/compare%20accuracy-1.png) Hence, the LDA is the most accurate. I'll zoom in on just this model.

``` r
# summarise Best Model
print(fit.lda)
```

    ## Linear Discriminant Analysis 
    ## 
    ## 120 samples
    ##   4 predictor
    ##   3 classes: 'setosa', 'versicolor', 'virginica' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 108, 108, 108, 108, 108, 108, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa
    ##   0.9833333  0.975

Using the LDA model, I want to get an idea of the accuracy of the model on the validation set, to quantify how well my choices have performed.

``` r
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
```

    ## Confusion Matrix and Statistics
    ## 
    ##             Reference
    ## Prediction   setosa versicolor virginica
    ##   setosa         10          0         0
    ##   versicolor      0         10         1
    ##   virginica       0          0         9
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9667          
    ##                  95% CI : (0.8278, 0.9992)
    ##     No Information Rate : 0.3333          
    ##     P-Value [Acc > NIR] : 2.963e-13       
    ##                                           
    ##                   Kappa : 0.95            
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: setosa Class: versicolor Class: virginica
    ## Sensitivity                 1.0000            1.0000           0.9000
    ## Specificity                 1.0000            0.9500           1.0000
    ## Pos Pred Value              1.0000            0.9091           1.0000
    ## Neg Pred Value              1.0000            1.0000           0.9524
    ## Prevalence                  0.3333            0.3333           0.3333
    ## Detection Rate              0.3333            0.3333           0.3000
    ## Detection Prevalence        0.3333            0.3667           0.3000
    ## Balanced Accuracy           1.0000            0.9750           0.9500

This is 100% accurate! This meets the expected criterion of 97 +- 4% for a model, suggesting this is a suitable choice for our dataset. Using this model, I can classify the species of any iris, based on the 4 measurements needed, with perfect accuracy.

///

To conclude, I have followed a tutorial guiding me through the principles of a machine learning project. I have explored and visualised a dataset, created 5 ML models, evaluated their accuracy, and verified that my chosen model is suitable. I now have the skills and the framework to perform further analyses on other data, using other models.
