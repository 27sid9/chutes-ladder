# chutes-ladder
04-01-2023
#run this cell to clear R's brain.
rm(list=ls())
##Setting up Libraries

library(tidyverse)
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
## ✔ tibble  3.1.6     ✔ dplyr   1.0.8
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
library(ggthemes)
library(scico)
library(readxl)
library(gganimate)
library(tidyr)
######1 Snakes and Ladders######## ##simulate a game by keeping player position, number of rolls, and the number of snakes and ladders encountered.

#Creating the chutes and ladder board.

board <-rbind( #combine multiple rows to form a single batch.
  
  sort(91:100, decreasing=TRUE), #sorting vector in descending order
  81:90,
  sort(71:80, decreasing=TRUE),
  61:70,
  sort(51:60, decreasing=TRUE),
  41:50,
  sort(31:40, decreasing=TRUE),
  21:30,
  sort(11:20, decreasing=TRUE),
  1:10 ) 

#The ladders and chutes will be set up as a data frame, allowing to quickly look up the board position and where to move the current location of the piece. 

ladder.df <- data.frame(start=c(1,4,9,21,28,36,51,71,80), #creating ladder start and end point
                        end=c(38,14,31,42,84,44,67,91,100))

chutes.df <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16),#creating snakes start and end point
                        end=c(78,75,73,24,60,19,53,11,26,6))
##1.1 Commenting Code

#Assigning parameters limits

max_turns <- 30  # number of turns to take
position <- 0  # start on the board
max_position <- 60 # number of box on board
n_sides_die <- 6 # number of sides on one die roll

                            
for (turn in 1:max_turns){      # use a for loop to simulate a number of turns from 1 to max turn
  die_roll <- sample.int(n_sides_die, 1) # simulate the values from rolls of an unbiased six-sided die
  position <- position + die_roll # compute new board position
  if ( position >= max_position ){  # break out of loop if we roll die that is greater than or equal to 60
    break
  }
}
##1.2 Single Player Simulation

ndie_roll <- 0 # Number of die rolls
chutes <- 0 # Number of chutes encountered
ladders <- 0 # Number of ladders encountered
position <- 0 # Current location/position


# Keep rolling dice and moving until reach 100 or greater ending the game
while(position < 100) {
  die_roll <- sample(1:6,1) # generate random number between [1 to 6]
  position <- position + die_roll # increase position
  ndie_roll <- ndie_roll + 1 # increase number of rolls
  
  # Need to check if we landed on a ladder or chutes and move forward or back
  if (any(ladder.df$s %in% position)) {
    position <- ladder.df$e[ladder.df$s %in% position]
    ladders <- ladders + 1
  }
  if (any(chutes.df$s %in% position)) {
    position <- chutes.df$e[chutes.df$s %in% position]
    chutes <- chutes + 1
  }
}
##1.3 Estimate the average position reached by a single player after 10 turns ## Ploting histogram

## Start and end positions of the snakes and ladders
ladders.df <- data.frame(start=c(1,4,9,21,28,36,51,71,80), 
                         end=c(38,14,31,42,84,44,67,91,100)) 
chutes.df <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16), 
                        end=c(78,75,73,24,60,19,53,11,26,6)) 
# We only need a vector of start squares and a vector of end squares
to <- c(chutes.df$end, ladders.df$end)
from <- c(chutes.df$start, ladders.df$start)


# This function takes the square you land on then applies:
# the reflection if more than 100
# the snakes and ladders
position <- function(x){
  if(x>100) x=100-(x-100)
  if(x %in% from) x <- to[which(from==x)]
  x
}

# Now set up the transition matrix. The probability of each dice roll number is 1/6
# Initialise a zero matrix
# Adding a position 101 which will be the starting 'zero' position.
P = matrix(0,nrow=101,ncol=101)
# Add 1/6 probability to every possible transition
for(i in 1:99){
  for(j in 1:6){
    P[i,position(i+j)]=P[i,position(i+j)]+1/6
  }
} 
# 100 is an absorbing state
P[100,100]=1
# Setting the rules for the first turn
for(j in 1:6) P[101,position(j)] <- P[101,position(j)]+1/6

#Now to simulate a path of 10 turns.
#Use the fact that the position on the 10th turn has a multinomial distribution with probabilities given by P[x,] where x is the position on the 9th turn (etc).

# Start at square 0
pos <- 101

# Turn 1 comes from a multinomial distribution with probabilities P[pos[101],]
pos[2] <- which(rmultinom(1,1,prob = P[pos[1],] )==1)

# We can add more turns (say 10)
# Start at square 0 (matrix row 101)
pos <- 101
for(i in 2:11){
  pos[i] <- which(rmultinom(1,1,prob = P[pos[i-1],] )==1)
}

# Look at the path (without the starting 0):
pos[-1]
##  [1]  6  8 12  6  8 31 35 38 41 43
#Finally, to plot a histogram of the position reached after ten turns:
# 1 turn transition matrix
P2 <- P
# Add 9 more turns to get the 10 turn transition matrix
for(i in 1:9) P2 <- P2 %*% P

# Sample 10000 times from the distribution starting from 0 after 10 turns:
sims <- sample(1:100,10000,TRUE,prob = P2[101,1:100])

# Plot histogram
hist(sims, breaks=100)


######Data Analysis######## ##2.1 Principal Component Analysis

library("ggplot2")
load("assessment_pca22.Rdata")#pulling the data 

pca_res <- prcomp(pca_dat[1:20, ], center = TRUE, scale = TRUE) #assigning data to pca_res by taking in the data from pca_dat and storing it as input in rows and setting center and scale arguments to TRUE.

pc_df <- data.frame(pca_res$x, grp = dat_group[1:20])# creating data frame by accessing variables from pca_res to x and dat_group by assigning name as grp in rows

ggplot(pc_df, aes(x = PC1, y = PC2, col = grp)) +
geom_point(size = 3) #creating ggplot by observing which samples cluster together.


##use principal component analysis (PCA) to perform dimensionality reduction on the full data matrix pca_dat

#print(pca_dat)
dim(pca_dat) #to get the dimensions of the data.
## [1]   28 4783
######2.2 PCA analysis####### #Plot a scatter plot in the first two principal components.

pca_res <- prcomp(pca_dat, center = TRUE, scale = TRUE) #now we can perform to see a description of the function and some help on possible arguments. Here we set center and scale arguments to TRUE
pca_res_summary <- summary(pca_res) #summarise the results from PCA
print(pca_res_summary) #printing the output pca_res_summary
## Importance of components:
##                            PC1     PC2      PC3      PC4      PC5     PC6
## Standard deviation     44.9201 29.9214 21.19188 15.88492 13.50954 12.1372
## Proportion of Variance  0.4219  0.1872  0.09389  0.05276  0.03816  0.0308
## Cumulative Proportion   0.4219  0.6090  0.70295  0.75570  0.79386  0.8247
##                            PC7     PC8     PC9    PC10    PC11    PC12    PC13
## Standard deviation     9.66961 9.05666 8.72461 7.55892 7.38764 7.10968 6.93541
## Proportion of Variance 0.01955 0.01715 0.01591 0.01195 0.01141 0.01057 0.01006
## Cumulative Proportion  0.84421 0.86136 0.87727 0.88922 0.90063 0.91120 0.92125
##                           PC14   PC15    PC16    PC17   PC18    PC19    PC20
## Standard deviation     6.79395 6.3006 6.04230 5.73830 5.5750 5.53666 5.40686
## Proportion of Variance 0.00965 0.0083 0.00763 0.00688 0.0065 0.00641 0.00611
## Cumulative Proportion  0.93090 0.9392 0.94684 0.95372 0.9602 0.96663 0.97274
##                           PC21    PC22    PC23    PC24    PC25    PC26    PC27
## Standard deviation     5.21905 5.04797 4.61387 4.30795 3.86430 3.72891 2.99616
## Proportion of Variance 0.00569 0.00533 0.00445 0.00388 0.00312 0.00291 0.00188
## Cumulative Proportion  0.97844 0.98376 0.98821 0.99209 0.99522 0.99812 1.00000
##                             PC28
## Standard deviation     3.179e-14
## Proportion of Variance 0.000e+00
## Cumulative Proportion  1.000e+00
pc_df <- data.frame(pca_res$x, grp = dat_group)

ggplot(pc_df, aes(x = PC1, y = PC2, col = grp)) + #creating ggplot by observing which samples cluster together
    geom_point(size = 3)


##Compute the variance explained by each principal component and visualise them in a barplot.

#Next we perform PCA on the data and extract the proportion of variance explained by each component.
pr_var <- pca_res$sdev^2 ## pr_var or variance is the square of the standard deviation and here we are pulling data from pca_res
prop_var_exp <- pr_var / sum(pr_var) # compute the variance by dividing total pr_var explained by each principal component
#We can visualise this as below
var_exp <- data.frame(variance = prop_var_exp, pc = 1:length(prop_var_exp))

ggplot(var_exp[1:30, ], aes(x = pc, y = variance)) + #creating bar plot to visualize the variance from hightest to lowest.
    geom_bar(stat = "identity") +
    labs(x = "Principal Component",
         y  = "Variance explained")
## Warning: Removed 2 rows containing missing values (position_stack).
 ##Identify the number of principal components you would need to count to explain 90 of the variance.

print("principal components-PC11 accounts for  90 of the variance") #after summarising the results from PCA we get the following variance-based on the output pca_res_summary
## [1] "principal components-PC11 accounts for  90 of the variance"
#run this cell to clear R's brain.
rm(list=ls())
##2.3 Multiple Regression

#load the data
load("assess_data_22.Rdata")
#For this part we will use the inbuilt bmi dataset containing height, weight and sex data for 3200 obs of 10 variables.

#by using linear regression initially,we can consider BMI to be the response variable and height and weight to be the features

#selecting library to convert units for calculation of real BMI as weight should be in kg and height should be in m.
library(measurements)

bmi_df$height_in_m <- measurements::conv_unit(bmi_df$height, "cm", "m")#converting height from cm to m to calculate bmi
bmi_df$height_in_m <- round(bmi_df$height_in_m, digits = 2)#selecting only 2 digits after decimal
bmi_df
## # A tibble: 3,200 × 11
##      idx   age   bai   bmi body_fat density weight height   hip sex   
##    <dbl> <dbl> <dbl> <dbl>    <dbl>   <dbl>  <dbl>  <dbl> <dbl> <chr> 
##  1  2611    23  17.3  15.8      6      1.09   5.39   170.  72.6 male  
##  2  2656    21  17.9  17.2      6.6    1.08   5.78   172.  77.9 male  
##  3  2452    24  18.5  17.6      5      1.09   5.91   166.  79.6 male  
##  4  1264    43  18.4  18.3     23.5    1.05   6.02   169.  81.0 male  
##  5   730    43  22.1  16.2     21.9    1.06   6.16   164.  82.9 female
##  6  3129    43  22.1  16.2     21.9    1.06   6.16   161.  83.0 female
##  7    44    23  20.8  18.2      9.4    1.08   6.16   171.  83.0 male  
##  8  2044    28  18.5  18.4      5.4    1.09   6.17   172.  83.0 male  
##  9  2219    41  18.4  18.4     18.4    1.06   6.17   169.  83.1 male  
## 10  2693    25  21.1  18.2     12.8    1.07   6.17   168.  83.1 male  
## # … with 3,190 more rows, and 1 more variable: height_in_m <dbl>
#similarly converting weight in kg to calculate bmi
bmi_df$weight_in_kg <- measurements::conv_unit(bmi_df$weight, "stone", "kg")
bmi_df$weight_in_kg <- round(bmi_df$weight_in_kg, digits = 2)
bmi_df
## # A tibble: 3,200 × 12
##      idx   age   bai   bmi body_fat density weight height   hip sex   
##    <dbl> <dbl> <dbl> <dbl>    <dbl>   <dbl>  <dbl>  <dbl> <dbl> <chr> 
##  1  2611    23  17.3  15.8      6      1.09   5.39   170.  72.6 male  
##  2  2656    21  17.9  17.2      6.6    1.08   5.78   172.  77.9 male  
##  3  2452    24  18.5  17.6      5      1.09   5.91   166.  79.6 male  
##  4  1264    43  18.4  18.3     23.5    1.05   6.02   169.  81.0 male  
##  5   730    43  22.1  16.2     21.9    1.06   6.16   164.  82.9 female
##  6  3129    43  22.1  16.2     21.9    1.06   6.16   161.  83.0 female
##  7    44    23  20.8  18.2      9.4    1.08   6.16   171.  83.0 male  
##  8  2044    28  18.5  18.4      5.4    1.09   6.17   172.  83.0 male  
##  9  2219    41  18.4  18.4     18.4    1.06   6.17   169.  83.1 male  
## 10  2693    25  21.1  18.2     12.8    1.07   6.17   168.  83.1 male  
## # … with 3,190 more rows, and 2 more variables: height_in_m <dbl>,
## #   weight_in_kg <dbl>
##We will now consider a linear regression example with multiple covariates, age as well as weight. In this case of course we know that they are related so we do expect both covariates to be significant.

#linear regression
lr_fit <- lm(bmi ~ weight_in_kg, data = bmi_df)
summary(lr_fit)
## 
## Call:
## lm(formula = bmi ~ weight_in_kg, data = bmi_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.2009 -1.4457 -0.5644  1.5272  4.5972 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.289358   0.166630   7.738 1.35e-14 ***
## weight_in_kg 0.404715   0.002764 146.428  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.666 on 3198 degrees of freedom
## Multiple R-squared:  0.8702, Adjusted R-squared:  0.8702 
## F-statistic: 2.144e+04 on 1 and 3198 DF,  p-value: < 2.2e-16
#multiple regression
mr_fit <- lm(bmi ~ height_in_m + weight_in_kg + sex, data = bmi_df)
summary(mr_fit)
## 
## Call:
## lm(formula = bmi ~ height_in_m + weight_in_kg + sex, data = bmi_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.2094 -0.4286 -0.0012  0.4284  2.6413 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.504652   0.940372   0.537    0.592    
## height_in_m  -0.708264   0.585681  -1.209    0.227    
## weight_in_kg  0.413198   0.001102 374.886   <2e-16 ***
## sexmale       3.141340   0.063301  49.625   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.663 on 3196 degrees of freedom
## Multiple R-squared:  0.9795, Adjusted R-squared:  0.9794 
## F-statistic: 5.078e+04 on 3 and 3196 DF,  p-value: < 2.2e-16
##Plot the residuals. Are they normally distributed?

library(MASS)
## 
## Attaching package: 'MASS'
## The following object is masked from 'package:dplyr':
## 
##     select
residuals_df <-data.frame(residual1 = residuals(mr_fit))

ggplot(residuals_df, aes(x = residual1)) +
    geom_histogram(bins = 10) 


#histogram is normally distributed
##(2.4) Which other variables are useful features for this model? Choose an appropriate algorithm to choose the features to include.

#First, we can test how each variable performs separately.
age.mod <- lm(bai ~ age, data = bmi_df)
body_fat.mod <- lm(bai ~ body_fat, data = bmi_df)
hip.mod <- lm(bai ~ hip, data = bmi_df) 

#Next, we want to know if the combination of age and body fat are better at describing variation in Bai on their own, without including beverage consumption.
age.body_fat.mod <- lm(bai ~ age + body_fat, data = bmi_df)

#We also want to know whether the combination of age, sex, and hip circumference is better at describing the variation in Bai than any of the previous models.
hip.mod <- lm(bai ~ age + body_fat + hip, data = bmi_df)

#Finally, we can check whether the interaction of age, body_fat, and beverage consumption can explain #BMI better than any of the previous models.
interaction.mod <- lm(bai ~ age*body_fat*hip, data = bmi_df)
#To compare these models and find which one is the best fit for the data use package AIC
#install.packages(AICcmodavg)
library(AICcmodavg)

#Then put the models into a list (‘models’) and name (label) each of them so the AIC table is easier to read #(‘model.names’).
models <- list(age.mod, body_fat.mod, hip.mod, age.body_fat.mod, hip.mod, interaction.mod)

model.names <- c('age.mod', 'body_fat.mod', 'hip.mod', 'age.body_fat.mod', 'hip.mod', 'interaction.mod')

#Finally, run aictab() to do the comparison.
aictab(cand.set = models, modnames = model.names)
## Warning in aictab.AIClm(cand.set = models, modnames = model.names): 
## Check model structure carefully as some models may be redundant
## 
## Model selection based on AICc:
## 
##                  K     AICc Delta_AICc AICcWt Cum.Wt       LL
## interaction.mod  9 15714.59       0.00      1      1 -7848.27
## hip.mod          5 15837.96     123.37      0      1 -7913.97
## hip.mod          5 15837.96     123.37      0      1 -7913.97
## body_fat.mod     3 16985.65    1271.06      0      1 -8489.82
## age.body_fat.mod 4 16987.15    1272.56      0      1 -8489.57
## age.mod          3 19357.01    3642.42      0      1 -9675.50
