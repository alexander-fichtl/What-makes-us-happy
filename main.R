


# What makes us happy - A young people dataset analysis

# Final class project for the 373290: Introduction to Data Science (Spring 2020) course

# By Alexander Fichtl, 14.06.2020




# Install packages
install.packages("GGally")
install.packages("dplyr")
install.packages("naniar")
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("corrr")
install.packages("fastDummies")
install.packages("caret")
install.packages("useful")
devtools::install_github("laresbernardo/lares")
install.packages("rattle")
install.packages("nnet")
install.packages("ggplot2")
install.packages("neuralnet")


# Load libraries
library(dplyr)
library(tidyverse)
library(skimr)
library(visdat)
library(DataExplorer)
library(naniar)
library("Hmisc")
library(tidyr)
library(GGally)
library(ggplot2)
library(corrr)
library(fastDummies)
library(lares) 
library(caret)
library(useful)
library(rattle)
library(nnet)
library(neuralnet)

dev.new()

#import data
data <- read.csv(file = 'responses.csv')

# --------------------------- Basic EDA ---------------------------


# get a feel for the actual data values by using head() and tail()
head(data) 
tail(data)

# use skim() to get a first overview of the dataset
skim(data)
   
# -> Number of rows             1010  
# -> Number of columns          150   
# Column type frequency:           
# -> factor: 11    
# -> numeric: 139 

# We use summary() three times: For numeric data, for categorical data, 
# and our variables of interest(all numeric) on their own.

# assign numeric data in data num
data_num <- dplyr::select_if(data, is.numeric)

# Summarize the numerical data
summary(data_num)

# Assign the categorical data to data_cat
data_cat <- dplyr::select_if(data, is.character)

# Summarize the categorical data
summary(data_cat)

# Fetching the data of interest
data_of_interest <-  data %>%
  select("Changing.the.past","Happiness.in.life","Religion","Science.and.technology")

# Sumarize that data
summary(data_of_interest)

# Check out the null  values in data
table(is.na (data))

# use the vidat library to get a better understanding of...
# ...the distibution of missing values
vis_miss(data)
# ... variable types
vis_dat(data)

# because of the large number of colums, the  functions used above can only tell us so much. 
# Moreover, everything is very crammed together. Lets try another approach:

# Plot the missing data to know in which section the peaks are
barplot(sapply(data, function(x) sum(is.na(x))))

# Cummlatively check out the missing data
sapply(data_of_interest, function(x) sum(is.na(x)))

# It looks like the weight and height column have the most missing values (20 each), 
# but even that accounts only to below 2%. We will not have to drop any colums and
# can easily apply data imputation later on. 

# (Height and Weight might be leading the list here because people were afraid of getting 
# recognized amongst the participants despite the survey being anonymous. 
# The participants were sharing a lot of very personal information by answering the questions, 
# after all. But this is just speculation.)

barplot(sapply(data_of_interest, function(x) sum(is.na(x))),names.arg = c("Changing.the.past","Happiness in Life","Religion","Science.and.technology"))

# Our variables of interest have very few missing values (>1%). We will impute them later on.

# For an even better vizualisation we can use the very comprehensive DataExplorer create_report()
# function. It creates the following plots/statistics:

# Basic Statistics -> Raw Counts, Percentages
# Data Structure
# Missing Data Profile
# Univariate Distribution -> Histogram, Bar Chart (by frequency), QQ Plot
# Correlation Analysis
# Principal Component Analysis

# DataExplorer::create_report(df)

# This report confirms a lot of the insights we have made so far. It shows that 
# Age, Children or Height and Weight are the only real continious variables. 
# The correlation matrix is too crammed to properly analize it, but it does show that there definately 
# are significant correlations amongst the variables of the dataset. Positive and negative. The most
# significant correlations seem to be right above/belov the diagonal. The clustering of the colums explains
# this: The first couple of colums all display variables regarding music preferences. And if someone
# likes music in general, he will probably listen to more than one genre. Hence the proximity of positive 
# correlations right around the first couple of variables.


# --------------------------- First Data Cleansing ---------------------------


# As shown above, we do not have to drop any columns because of missing values. 
# Still, since we have already defined our variables of interest, we should drop rows 
# that are missing values for each of the four ("Changing the past", "Happiness in life", 
# "Religion", and "Science and technology"). Let's see if this is the case for any rows:

tmpdf <- data[is.na(data$Happiness.in.life) & is.na(data$Science.and.technology) 
              & is.na(data$Changing.the.past) & is.na(data$Religion),]
count(tmpdf)

# No rows miss all of the 4 variables at the same time. This means, all of the rows in the 
# dataset have some kind of value to us.

# Next we will check for outliers that disturb the analysis/visualizations. 
# Since weight, height, and age are the only "open-ended" integer variables, 
# they are the only ones that allow real outliers. Therefore we only have to check those three.

# Horizontal violin plot of gender vs height 
ggplot(data, aes(x=Height, y=Gender)) + 
  geom_violin()

# Horizontal violin plot of gender vs weight 
ggplot(data, aes(x=Weight, y=Gender)) + 
  geom_violin()

# Interleaved histograms of Age vs gender
ggplot(data, aes(x=Age, fill=Gender)) +
  geom_histogram(binwidth=2, position="dodge")

# While everthing seems fine with the age responses, there are some height and weight 
# outliers that really strech the violinplots. Lets take a closer look at them.

# Fetch the data of interest 2 for plotting graph
data_of_interest2 <-  data %>%
  select("Age","Height","Weight","Gender","Changing.the.past","Happiness.in.life","Religion","Science.and.technology")

# Filter the data with respect to height which is less than 70
filter(data_of_interest2, data$Height<70)

# Filter the data with respect to weight which is greater than 120
filter(data_of_interest2, data$Weight>120)

#While the rows with 62cm in height and 165kg in weight might have been errors in the dataset, 
# the participants might also have just mistaken the field format. 62cm could have meant 162cm 
# instead. And since the variables of interest values look just fine, we will keep the rows.

# --------------------------- Main EDA Section ---------------------------

# Age distributions amongst the variables:
# Ive spent several hours trying to get a plot that displays the percentage of a specic age/gender per variable value.
# In the end though, I only managed to get the frequencies, not the percentages. This does not provide any insights
# though, because there are way more females in the study than males. I then tried my luck with python and
# finally manged to find a solution with the seaborn library. The code is in the jypter notebook which I will also upload.
# I will include the plots in the data story and comment on them.


### Correlations

# In order to plot a more easily understandable correlation matrix than in the data report we can use ggcorr from the GGally package
# and use its parameters to highlight correlations with significant coefficients (here |coeff| > 0.3)

# First we create dummies for the categorical variabels. Otherwise, we cant plot anything.

data_with_dummies <- dummy_cols(data, remove_selected_columns = TRUE)

ggcorr(data_with_dummies, geom = "blank", label = TRUE, hjust = 0.75) +
  geom_point(size = 10, aes(color = coefficient < 0, alpha = abs(coefficient) > 0.3)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# This plot shows that there is a fair amount of significant correlations between the variables. Luckily,
# the variables I am most interested in (see Data Story) are no exception here. Lets take a closer look 
# at the highest general correlations (we can use the lares library for this, this way we can also check the p-values):

corr_cross(data_with_dummies, # dataset
           max_pvalue = 0.05, # show only sig. correlations at selected level
           top = 20) # display top 20 correlations, any couples of variables

# A closer look at the correlations with the Gender variable:

corr_var(data_with_dummies, # dataset
         Gender_male, # name of variable to focus on
         top = 20) # display top 10 correlations

# Next we will plot the most significant correlations for each variable of interest.

# Changing the past
         
x <- data_with_dummies %>% 
  correlate() %>% 
  focus(Changing.the.past)

x <- subset(x, Changing.the.past > 0.1 | Changing.the.past < -0.1)  

x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(Changing.the.past)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = Changing.the.past, fill=Changing.the.past)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Changing the past") +
  xlab("Variable") + coord_flip()

# Happiness in life

x <- data_with_dummies %>% 
  correlate() %>% 
  focus(Happiness.in.life)

x <- subset(x, Happiness.in.life > 0.1 | Happiness.in.life < -0.1)  

x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(Happiness.in.life)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = Happiness.in.life, fill=Happiness.in.life)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Happiness in life") +
  xlab("Variable") + coord_flip()

# Religion

x <- data_with_dummies %>% 
  correlate() %>% 
  focus(Religion)

x <- subset(x, Religion > 0.1 | Religion < -0.1)  

x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(Religion)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = Religion, fill=Religion)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Religion") +
  xlab("Variable") + coord_flip()

# Science and technology

x <- data_with_dummies %>% 
  correlate() %>% 
  focus(Science.and.technology)

x <- subset(x, Science.and.technology > 0.1 | Science.and.technology < -0.1)  

x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(Science.and.technology)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = Science.and.technology, fill=Science.and.technology)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Science and technology") +
  xlab("Variable") + coord_flip()


# --------------------------- Machine Learning ---------------------------

### Prepararation of the dataset


# Again we create a dataframe with dummy variables. This time we remove the first dummy of every variable such that 
# only n-1 dummies remain. This avoids multicollinearity issues in models.

ml_data <- dummy_cols(data, remove_selected_columns = TRUE, remove_first_dummy = TRUE)
skim(ml_data)


# Data split and imputation 

# first we have to fix the dummy column names. They contain spaces that cant be parsed:
cols <- colnames(ml_data)
cols2 <- make.names(cols)
colnames(ml_data) <- cols2

# Imputation
ml_data <- simple.impute(ml_data, mean)

# create separate dataset for neural network later on 

nn_ml_data <- ml_data

ml_data$Happiness.in.life <- as.factor(ml_data$Happiness.in.life)

# Split into train/test set

train <- sample_frac(ml_data, 0.8)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- ml_data[-sample_id,]

train$Happiness.in.life <- relevel(train$Happiness.in.life, ref = 3)


# Training the multinomial model with the variables with highest correlation 
multinom.fit <- multinom(Happiness.in.life ~ Energy.levels + Number.of.friends + Personality + Loneliness 
                         + Changing.the.past + Interests.or.hobbies + Dreams + Mood.swings + Fear.of.public.speaking #
                         + Fake + Fun.with.friends, data = train)

# Checking the model
summary(multinom.fit)

# We can use probabilities to understand our model:
head(probability.table <- fitted(multinom.fit))

# Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")

# Building classification table
ctable <- table(train$Happiness.in.life, train$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

# Accuracy in training dataset is 57.18%. We now repeat the above on the unseen dataset that tests dataset.

# Predicting the values for train dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")

# Building classification table
ctable <- table(test$Happiness.in.life, test$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

# -> 50.5%

# The accuracy of the test dataset turns out to be around 7% less as compared to training dataset. So we have
# a problem of overfitting here. There are several approaches to improve the model: 
# -> 1. Look for multicollinearities and remove variable or b. Run factor analysis

# -> 2. Check for outliers and do the necessary treatment.

# -> 3. Try different data transformations for independent variables.

# For now though, we will move on and try a different model: a neural network

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(nn_ml_data, normalize))

# make train/test split with 80% of the sample size
smp_size <- floor(0.8 * nrow(maxmindf))

# set the seed to make partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(maxmindf)), size = smp_size)

train <- maxmindf[train_ind, ]
test <- maxmindf[-train_ind, ]

# fit neural network
nn=neuralnet(Happiness.in.life ~ Energy.levels + Number.of.friends + Personality + Loneliness 
             + Changing.the.past + Interests.or.hobbies + Dreams + Mood.swings + Fear.of.public.speaking #
             + Fake + Fun.with.friends, data=train, hidden=c(5,2),
             linear.output = FALSE, threshold=0.01)

# plot neural network
plot(nn)

## Prediction using neural network
nn.results <- compute(nn, test)
results <- data.frame(actual = test$Happiness.in.life, prediction = nn.results$net.result)
results

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

# unfortunately,something went wrong and I did not have the time to find my mistake. Instead, I wanted to train a Random Forest
# in Python, because here I do have experience and I wanted to see if good performance is actually possible. In the jupyter-
# notebook I will also give oversampling with SMOTE (The happiness variable is highly unbalanced) and parameter tuning a shot.

### END