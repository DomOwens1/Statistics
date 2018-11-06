Classification of Football Players by Position
================

/// Credit again to machinelearningmastery.com/machine-learning-in-r-step-by-step/ ///

Building on my previous project, I will construct a multi-variable regression model with the intent of classifying a dataset of football players by their playing positions, using data about their characteristics (obtained from the Fifa video game database - let's assume it is accurate for the purposes of this project).

``` r
#load the caret package (http://topepo.github.io/caret/index.html)
library(lattice)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.1

    ## -- Attaching packages ----------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts -------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.5.1

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
# load in the dataset
dataset <- read_csv("~/stats/hw3/complete.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_logical(),
    ##   `)` = col_integer(),
    ##   name = col_character(),
    ##   full_name = col_character(),
    ##   club = col_character(),
    ##   club_logo = col_character(),
    ##   special = col_integer(),
    ##   age = col_integer(),
    ##   league = col_character(),
    ##   birth_date = col_character(),
    ##   height_cm = col_integer(),
    ##   weight_kg = col_integer(),
    ##   body_type = col_character(),
    ##   flag = col_character(),
    ##   nationality = col_character(),
    ##   photo = col_character(),
    ##   eur_value = col_integer(),
    ##   eur_wage = col_integer(),
    ##   eur_release_clause = col_integer(),
    ##   overall = col_integer(),
    ##   potential = col_integer()
    ##   # ... with 73 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
# take a look, find the dimensions
head(dataset)
```

    ## # A tibble: 6 x 185
    ##      `)` name   full_name  club  club_logo special   age league birth_date
    ##    <int> <chr>  <chr>      <chr> <chr>       <int> <int> <chr>  <chr>     
    ## 1  20801 Crist~ C. Ronald~ Real~ https://~    2228    32 Spani~ 05/02/1985
    ## 2 158023 L. Me~ Lionel Me~ FC B~ https://~    2158    30 Spani~ 24/06/1987
    ## 3 190871 Neymar Neymar da~ Pari~ https://~    2100    25 Frenc~ 05/02/1992
    ## 4 176580 L. Su~ Luis Suár~ FC B~ https://~    2291    30 Spani~ 24/01/1987
    ## 5 167495 M. Ne~ Manuel Ne~ FC B~ https://~    1493    31 Germa~ 27/03/1986
    ## 6 188545 R. Le~ Robert Le~ FC B~ https://~    2146    28 Germa~ 21/08/1988
    ## # ... with 176 more variables: height_cm <int>, weight_kg <int>,
    ## #   body_type <chr>, real_face <lgl>, flag <chr>, nationality <chr>,
    ## #   photo <chr>, eur_value <int>, eur_wage <int>,
    ## #   eur_release_clause <int>, overall <int>, potential <int>, pac <int>,
    ## #   sho <int>, pas <int>, dri <int>, def <int>, phy <int>,
    ## #   international_reputation <int>, skill_moves <int>, weak_foot <int>,
    ## #   work_rate_att <chr>, work_rate_def <chr>, preferred_foot <chr>,
    ## #   crossing <int>, finishing <int>, heading_accuracy <int>,
    ## #   short_passing <int>, volleys <int>, dribbling <int>, curve <int>,
    ## #   free_kick_accuracy <int>, long_passing <int>, ball_control <int>,
    ## #   acceleration <int>, sprint_speed <int>, agility <int>,
    ## #   reactions <int>, balance <int>, shot_power <int>, jumping <int>,
    ## #   stamina <int>, strength <int>, long_shots <int>, aggression <int>,
    ## #   interceptions <int>, positioning <int>, vision <int>, penalties <int>,
    ## #   composure <int>, marking <int>, standing_tackle <int>,
    ## #   sliding_tackle <int>, gk_diving <int>, gk_handling <int>,
    ## #   gk_kicking <int>, gk_positioning <int>, gk_reflexes <int>, rs <int>,
    ## #   rw <int>, rf <int>, ram <int>, rcm <int>, rm <int>, rdm <int>,
    ## #   rcb <int>, rb <int>, rwb <int>, st <int>, lw <int>, cf <int>,
    ## #   cam <int>, cm <int>, lm <int>, cdm <int>, cb <int>, lb <int>,
    ## #   lwb <int>, ls <int>, lf <int>, lam <int>, lcm <int>, ldm <int>,
    ## #   lcb <int>, gk <int>, `1_on_1_rush_trait` <lgl>,
    ## #   acrobatic_clearance_trait <lgl>, argues_with_officials_trait <lgl>,
    ## #   avoids_using_weaker_foot_trait <lgl>, backs_into_player_trait <lgl>,
    ## #   bicycle_kicks_trait <lgl>, cautious_with_crosses_trait <lgl>,
    ## #   chip_shot_trait <lgl>, chipped_penalty_trait <lgl>,
    ## #   comes_for_crosses_trait <lgl>, corner_specialist_trait <lgl>,
    ## #   diver_trait <lgl>, dives_into_tackles_trait <lgl>,
    ## #   diving_header_trait <lgl>, driven_pass_trait <lgl>, ...

``` r
dim(dataset)
```

    ## [1] 17994   185

Gosh, what an unwieldy dataset! Nearly 18000 entries, and 185 attributes. First, I'll need to interpret the boolean table of preferred positions into the format I want, a categorical variable from (STR, MID, DEF, GK) representing where on the pitch the player plays. I'll add columns with the above names, containing a boolean variable denoting whether the player can play there.

``` r
#add a position column, wrangled from the boolean table of preferences
dataset <- dataset%>%
          mutate(STR = (prefers_rs == TRUE | prefers_ls == TRUE | prefers_st == TRUE | prefers_rf == TRUE | prefers_lf == TRUE | prefers_cf == TRUE), #find strikers
                 MID = (prefers_rw == TRUE | prefers_lw == TRUE | prefers_ram == TRUE | prefers_rcm == TRUE | prefers_rdm == TRUE | prefers_cam == TRUE | prefers_cm == TRUE | prefers_lm == TRUE | prefers_lam == TRUE | prefers_lcm == TRUE | prefers_ldm == TRUE), #find midfielders
                 DEF = (prefers_rwb == TRUE | prefers_lwb == TRUE | prefers_cb == TRUE | prefers_rcb == TRUE | prefers_lcb == TRUE | prefers_lb == TRUE | prefers_rb == TRUE),#find defenders
              GK = prefers_gk == TRUE) #find goalkeepers
head(dataset)
```

    ## # A tibble: 6 x 189
    ##      `)` name   full_name  club  club_logo special   age league birth_date
    ##    <int> <chr>  <chr>      <chr> <chr>       <int> <int> <chr>  <chr>     
    ## 1  20801 Crist~ C. Ronald~ Real~ https://~    2228    32 Spani~ 05/02/1985
    ## 2 158023 L. Me~ Lionel Me~ FC B~ https://~    2158    30 Spani~ 24/06/1987
    ## 3 190871 Neymar Neymar da~ Pari~ https://~    2100    25 Frenc~ 05/02/1992
    ## 4 176580 L. Su~ Luis Suár~ FC B~ https://~    2291    30 Spani~ 24/01/1987
    ## 5 167495 M. Ne~ Manuel Ne~ FC B~ https://~    1493    31 Germa~ 27/03/1986
    ## 6 188545 R. Le~ Robert Le~ FC B~ https://~    2146    28 Germa~ 21/08/1988
    ## # ... with 180 more variables: height_cm <int>, weight_kg <int>,
    ## #   body_type <chr>, real_face <lgl>, flag <chr>, nationality <chr>,
    ## #   photo <chr>, eur_value <int>, eur_wage <int>,
    ## #   eur_release_clause <int>, overall <int>, potential <int>, pac <int>,
    ## #   sho <int>, pas <int>, dri <int>, def <int>, phy <int>,
    ## #   international_reputation <int>, skill_moves <int>, weak_foot <int>,
    ## #   work_rate_att <chr>, work_rate_def <chr>, preferred_foot <chr>,
    ## #   crossing <int>, finishing <int>, heading_accuracy <int>,
    ## #   short_passing <int>, volleys <int>, dribbling <int>, curve <int>,
    ## #   free_kick_accuracy <int>, long_passing <int>, ball_control <int>,
    ## #   acceleration <int>, sprint_speed <int>, agility <int>,
    ## #   reactions <int>, balance <int>, shot_power <int>, jumping <int>,
    ## #   stamina <int>, strength <int>, long_shots <int>, aggression <int>,
    ## #   interceptions <int>, positioning <int>, vision <int>, penalties <int>,
    ## #   composure <int>, marking <int>, standing_tackle <int>,
    ## #   sliding_tackle <int>, gk_diving <int>, gk_handling <int>,
    ## #   gk_kicking <int>, gk_positioning <int>, gk_reflexes <int>, rs <int>,
    ## #   rw <int>, rf <int>, ram <int>, rcm <int>, rm <int>, rdm <int>,
    ## #   rcb <int>, rb <int>, rwb <int>, st <int>, lw <int>, cf <int>,
    ## #   cam <int>, cm <int>, lm <int>, cdm <int>, cb <int>, lb <int>,
    ## #   lwb <int>, ls <int>, lf <int>, lam <int>, lcm <int>, ldm <int>,
    ## #   lcb <int>, gk <int>, `1_on_1_rush_trait` <lgl>,
    ## #   acrobatic_clearance_trait <lgl>, argues_with_officials_trait <lgl>,
    ## #   avoids_using_weaker_foot_trait <lgl>, backs_into_player_trait <lgl>,
    ## #   bicycle_kicks_trait <lgl>, cautious_with_crosses_trait <lgl>,
    ## #   chip_shot_trait <lgl>, chipped_penalty_trait <lgl>,
    ## #   comes_for_crosses_trait <lgl>, corner_specialist_trait <lgl>,
    ## #   diver_trait <lgl>, dives_into_tackles_trait <lgl>,
    ## #   diving_header_trait <lgl>, driven_pass_trait <lgl>, ...

I'll condense these into a factor, stored in the column "position". This is the information I will use to classify the players later on. For instance, "1100" denotes a player is suited to being a striker or midfielder, but not a defender or goalkeeper, while "0001" is a goalkeeper.

``` r
#convert above columns into binary variables
dataset$STR <- as.integer(dataset$STR)
dataset$MID <- as.integer(dataset$MID)
dataset$DEF <- as.integer(dataset$DEF)
dataset$GK <- as.integer(dataset$GK)

#add position column
dataset <- dataset %>% 
  unite(col = position, STR, MID, DEF, GK, sep = "")%>%
  mutate(position = as.factor(position))

head(dataset)
```

    ## # A tibble: 6 x 186
    ##      `)` name   full_name  club  club_logo special   age league birth_date
    ##    <int> <chr>  <chr>      <chr> <chr>       <int> <int> <chr>  <chr>     
    ## 1  20801 Crist~ C. Ronald~ Real~ https://~    2228    32 Spani~ 05/02/1985
    ## 2 158023 L. Me~ Lionel Me~ FC B~ https://~    2158    30 Spani~ 24/06/1987
    ## 3 190871 Neymar Neymar da~ Pari~ https://~    2100    25 Frenc~ 05/02/1992
    ## 4 176580 L. Su~ Luis Suár~ FC B~ https://~    2291    30 Spani~ 24/01/1987
    ## 5 167495 M. Ne~ Manuel Ne~ FC B~ https://~    1493    31 Germa~ 27/03/1986
    ## 6 188545 R. Le~ Robert Le~ FC B~ https://~    2146    28 Germa~ 21/08/1988
    ## # ... with 177 more variables: height_cm <int>, weight_kg <int>,
    ## #   body_type <chr>, real_face <lgl>, flag <chr>, nationality <chr>,
    ## #   photo <chr>, eur_value <int>, eur_wage <int>,
    ## #   eur_release_clause <int>, overall <int>, potential <int>, pac <int>,
    ## #   sho <int>, pas <int>, dri <int>, def <int>, phy <int>,
    ## #   international_reputation <int>, skill_moves <int>, weak_foot <int>,
    ## #   work_rate_att <chr>, work_rate_def <chr>, preferred_foot <chr>,
    ## #   crossing <int>, finishing <int>, heading_accuracy <int>,
    ## #   short_passing <int>, volleys <int>, dribbling <int>, curve <int>,
    ## #   free_kick_accuracy <int>, long_passing <int>, ball_control <int>,
    ## #   acceleration <int>, sprint_speed <int>, agility <int>,
    ## #   reactions <int>, balance <int>, shot_power <int>, jumping <int>,
    ## #   stamina <int>, strength <int>, long_shots <int>, aggression <int>,
    ## #   interceptions <int>, positioning <int>, vision <int>, penalties <int>,
    ## #   composure <int>, marking <int>, standing_tackle <int>,
    ## #   sliding_tackle <int>, gk_diving <int>, gk_handling <int>,
    ## #   gk_kicking <int>, gk_positioning <int>, gk_reflexes <int>, rs <int>,
    ## #   rw <int>, rf <int>, ram <int>, rcm <int>, rm <int>, rdm <int>,
    ## #   rcb <int>, rb <int>, rwb <int>, st <int>, lw <int>, cf <int>,
    ## #   cam <int>, cm <int>, lm <int>, cdm <int>, cb <int>, lb <int>,
    ## #   lwb <int>, ls <int>, lf <int>, lam <int>, lcm <int>, ldm <int>,
    ## #   lcb <int>, gk <int>, `1_on_1_rush_trait` <lgl>,
    ## #   acrobatic_clearance_trait <lgl>, argues_with_officials_trait <lgl>,
    ## #   avoids_using_weaker_foot_trait <lgl>, backs_into_player_trait <lgl>,
    ## #   bicycle_kicks_trait <lgl>, cautious_with_crosses_trait <lgl>,
    ## #   chip_shot_trait <lgl>, chipped_penalty_trait <lgl>,
    ## #   comes_for_crosses_trait <lgl>, corner_specialist_trait <lgl>,
    ## #   diver_trait <lgl>, dives_into_tackles_trait <lgl>,
    ## #   diving_header_trait <lgl>, driven_pass_trait <lgl>, ...

Also, I'll group the data by position and visualise the counts of each category, to get a better idea of how the data looks.

``` r
#group by position class, count frequency
pos_count <- dataset %>%
  group_by(position)%>%
  summarise(n = n())

#plot as a bar chart, ordered by frequency
ggplot(pos_count, aes(x=reorder(position, -n), y=n)) +
  geom_bar(stat = "identity")
```

![](ML_project_2_files/figure-markdown_github/visualise-1.png)

I'll select only the columns I want for the purposes of my project.

``` r
dataset <- select(dataset, age, height_cm, weight_kg, eur_wage, position)
head(dataset, 20)
```

    ## # A tibble: 20 x 5
    ##      age height_cm weight_kg eur_wage position
    ##    <int>     <int>     <int>    <int> <fct>   
    ##  1    32       185        80   565000 1100    
    ##  2    30       170        72   565000 1100    
    ##  3    25       175        68   280000 0100    
    ##  4    30       182        86   510000 1000    
    ##  5    31       193        92   230000 0001    
    ##  6    28       185        79   355000 1000    
    ##  7    26       193        76   215000 0001    
    ##  8    26       173        76   295000 0100    
    ##  9    27       182        78   340000 0100    
    ## 10    29       184        87   275000 1000    
    ## 11    31       183        75   310000 0010    
    ## 12    26       181        68   285000 0100    
    ## 13    25       199        94   190000 0001    
    ## 14    28       169        62   265000 1100    
    ## 15    31       174        65   340000 0100    
    ## 16    27       183        74   370000 0100    
    ## 17    29       173        70   325000 1000    
    ## 18    32       187        85   225000 0010    
    ## 19    39       191        94   110000 0001    
    ## 20    23       177        73   215000 1100

I will need a dataset for validation of the model, and so I will partition the entire dataset into two parts, 80% for training and 20% for validation.

``` r
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$position, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
```

After that ordeal, Let's go back to basics and explore the data I have.

``` r
# dimensions of dataset
dim(dataset)
```

    ## [1] 14398     5

``` r
# list types for each attribute
sapply(dataset, class)
```

    ##       age height_cm weight_kg  eur_wage  position 
    ## "integer" "integer" "integer" "integer"  "factor"

``` r
# take a peek at the first 5 rows of the data
head(dataset)
```

    ## # A tibble: 6 x 5
    ##     age height_cm weight_kg eur_wage position
    ##   <int>     <int>     <int>    <int> <fct>   
    ## 1    32       185        80   565000 1100    
    ## 2    30       170        72   565000 1100    
    ## 3    30       182        86   510000 1000    
    ## 4    31       193        92   230000 0001    
    ## 5    28       185        79   355000 1000    
    ## 6    26       193        76   215000 0001

``` r
# list the levels for the class - i.e, position
levels(dataset$position)
```

    ## [1] "0000" "0001" "0010" "0100" "0110" "1000" "1010" "1100" "1110"

``` r
# summarize the class distribution
percentage <- prop.table(table(dataset$position)) * 100
cbind(freq=table(dataset$position), percentage=percentage)
```

    ##      freq  percentage
    ## 0000  577  4.00750104
    ## 0001 1617 11.23072649
    ## 0010 4495 31.21961384
    ## 0100 4309 29.92776775
    ## 0110  672  4.66731490
    ## 1000 1695 11.77246840
    ## 1010   17  0.11807195
    ## 1100 1008  7.00097236
    ## 1110    8  0.05556327

``` r
# summarize attribute distributions
summary(dataset)
```

    ##       age          height_cm       weight_kg         eur_wage     
    ##  Min.   :16.00   Min.   :155.0   Min.   : 49.00   Min.   :     0  
    ##  1st Qu.:21.00   1st Qu.:177.0   1st Qu.: 70.00   1st Qu.:  2000  
    ##  Median :25.00   Median :181.0   Median : 75.00   Median :  4000  
    ##  Mean   :25.11   Mean   :181.3   Mean   : 75.42   Mean   : 11534  
    ##  3rd Qu.:28.00   3rd Qu.:186.0   3rd Qu.: 80.00   3rd Qu.: 12000  
    ##  Max.   :47.00   Max.   :205.0   Max.   :110.00   Max.   :565000  
    ##                                                                   
    ##     position   
    ##  0010   :4495  
    ##  0100   :4309  
    ##  1000   :1695  
    ##  0001   :1617  
    ##  1100   :1008  
    ##  0110   : 672  
    ##  (Other): 602

Now, let's visualise the data and get a better look at it.

I'll start with univariate plots, to better understand the properties of each variable.

``` r
# split input and output - the measurements define x, and the position, y
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
  for(i in 1:4) {
  boxplot(x[,i], main=names(dataset)[i])
}
```

![](ML_project_2_files/figure-markdown_github/visualise%201-1.png) This tells me about the measurement distributions, and that the observations are equal in number.

///

Multivariate plots can tell me about the relationships between the different variables.

``` r
# scatterplot matrix - this creates a chart of each variable plotted against every other, allowing trend recognition
featurePlot(x=dataset[,1:4], y=dataset[,5], plot="pairs")
```

![](ML_project_2_files/figure-markdown_github/visualise%202-1.png)

``` r
# box and whisker plots for each attribute - as in the above boxplots, this allows distribution comparison, but also can identify clear differences between classes
#featurePlot(x=dataset[,1:4], y=dataset[,5], plot = "box")

# density plots for each attribute by class value - from this, I can see the distribution of each attribute in a smooth manner
#scales <- list(x=list(relation="free"), y=list(relation="free"))
#featurePlot(x=dataset[,1:4], y=dataset[,5], plot="density", scales=scales)
```

Now that I have a deeper understanding of the data properties, it's time to set up some ML algorithms for predicting the position of each player from his attributes.

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
fit.lda <- train(position~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(position~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(position~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
#fit.svm <- train(position~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
#fit.rf <- train(position~., data=dataset, method="rf", metric=metric, trControl=control)
```

And then compare them in terms of accuracy

``` r
#summarise accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn #svm=fit.svm, rf=fit.rf
                          ))
summary(results)
```

    ## 
    ## Call:
    ## summary.resamples(object = results)
    ## 
    ## Models: lda, cart, knn 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lda  0.4112735 0.4180851 0.4243044 0.4260314 0.4368311 0.4426685    0
    ## cart 0.3955586 0.4111517 0.4127640 0.4140880 0.4146425 0.4336345    0
    ## knn  0.3601666 0.3689496 0.3779554 0.3756112 0.3813308 0.3863794    0
    ## 
    ## Kappa 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lda  0.1661406 0.1764337 0.1858205 0.1875017 0.2027984 0.2116302    0
    ## cart 0.1430110 0.1622973 0.1689275 0.1696922 0.1735249 0.1986853    0
    ## knn  0.1189700 0.1319474 0.1442674 0.1403805 0.1479334 0.1552318    0

``` r
# compare accuracy of models
dotplot(results)
```

![](ML_project_2_files/figure-markdown_github/compare%20accuracy-1.png) Hence, the LDA is the most accurate. I'll zoom in on just this model.

``` r
# summarise Best Model
print(fit.lda)
```

    ## Linear Discriminant Analysis 
    ## 
    ## 14398 samples
    ##     4 predictor
    ##     9 classes: '0000', '0001', '0010', '0100', '0110', '1000', '1010', '1100', '1110' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 12959, 12959, 12956, 12956, 12961, 12957, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.4260314  0.1875017

Using the LDA model, I want to get an idea of the accuracy of the model on the validation set, to quantify how well my choices have performed.

``` r
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$position)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction 0000 0001 0010 0100 0110 1000 1010 1100 1110
    ##       0000    0    0    0    0    0    0    0    0    0
    ##       0001    4  101   79    5    0   38    0    5    0
    ##       0010   46  287  688  366   79  248    3   97    0
    ##       0100   94   16  356  706   88  137    1  150    2
    ##       0110    0    0    0    0    0    0    0    0    0
    ##       1000    0    0    0    0    0    0    0    0    0
    ##       1010    0    0    0    0    0    0    0    0    0
    ##       1100    0    0    0    0    0    0    0    0    0
    ##       1110    0    0    0    0    0    0    0    0    0
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4157          
    ##                  95% CI : (0.3996, 0.4321)
    ##     No Information Rate : 0.3123          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.1726          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0000 Class: 0001 Class: 0010 Class: 0100
    ## Sensitivity              0.00000     0.25000      0.6126      0.6555
    ## Specificity              1.00000     0.95896      0.5447      0.6649
    ## Pos Pred Value               NaN     0.43534      0.3793      0.4555
    ## Neg Pred Value           0.95996     0.90993      0.7559      0.8187
    ## Prevalence               0.04004     0.11235      0.3123      0.2995
    ## Detection Rate           0.00000     0.02809      0.1913      0.1963
    ## Detection Prevalence     0.00000     0.06452      0.5044      0.4310
    ## Balanced Accuracy        0.50000     0.60448      0.5787      0.6602
    ##                      Class: 0110 Class: 1000 Class: 1010 Class: 1100
    ## Sensitivity              0.00000      0.0000    0.000000     0.00000
    ## Specificity              1.00000      1.0000    1.000000     1.00000
    ## Pos Pred Value               NaN         NaN         NaN         NaN
    ## Neg Pred Value           0.95356      0.8824    0.998888     0.92992
    ## Prevalence               0.04644      0.1176    0.001112     0.07008
    ## Detection Rate           0.00000      0.0000    0.000000     0.00000
    ## Detection Prevalence     0.00000      0.0000    0.000000     0.00000
    ## Balanced Accuracy        0.50000      0.5000    0.500000     0.50000
    ##                      Class: 1110
    ## Sensitivity            0.0000000
    ## Specificity            1.0000000
    ## Pos Pred Value               NaN
    ## Neg Pred Value         0.9994438
    ## Prevalence             0.0005562
    ## Detection Rate         0.0000000
    ## Detection Prevalence   0.0000000
    ## Balanced Accuracy      0.5000000

This is only 42% accurate, meaning the majority of my test data has been misclassified. This does not meet the expected criterion of 97 +- 4% for a model, suggesting this is a not suitable choice for my dataset. I will need to look further into methods for classifying with larger numbers of possible classes, and perhaps consider condensing my position classes into fewer options (perhaps into 4, simply by str/mid/def/gk) to allow greater accuracy.

///

To conclude, I have adapted my previous ML classification project to a new dataset. I have wrangled the data into a usable format, explored and visualised the dataset, created 3 ML models, evaluated their accuracy, and evaluated the overall findings from my chosen model. I now greater insight into test design and the issues that can arise during classification problems.
