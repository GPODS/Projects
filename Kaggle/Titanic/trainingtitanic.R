
# Load packages

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

train <- read.csv('train.csv', stringsAsFactors = F) # tip:  'stringsAsFactors is a logical argument; should character vectors be converted to factors? The 'factory-fresh' default is TRUE
test  <- read.csv('test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

str(full) # Compactly display the internal structure of an object. Alternative to 'summary'.

# - - FEATURE ENGINEERING - - #

#Grab title from passenger names

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name) # Must learn

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again.
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

## We are going to make a family size variable based on number of spouses and number of children/parents.

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

ggplot(full[1:891,], aes(x = FsizeD, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Family Size') +
  theme_few()

## Now we'll focus in the passenger cabin variable. 

 # The first character appears to be the  deck, so we create a Deck variable.

full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# - - MISSING DATA - - #

## Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']

# But we do know how much their fare was
full[c(62, 830), 'Fare']

# And in which class they were
full[c(62, 830), 'Pclass']

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# We now can see that the passengers were in Class 1 and paid $80 for the tickets.
# In this visualization we notice that their fare coincides beautifuly with the median fare for the first class passenger departing from 'C'.
# Therefore, we shall replace the 'NA' values with 'C' for both passengers.

## Similarly, passenger on row 1044 has an 'NA' Fare value.
# Show row 1044
full[1044, ]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#09e8da', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# From this visualization, it seems quite reasonable to replace the 'NA' Fare value with median for their class and embarkment.

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


## - - PREDICTIVE IMPUTATION - - ##

## We can get more fancy in imputing missing values. We shall create a model predicting ages based on other variables.

# Show number of missing Age values
sum(is.na(full$Age))

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')


## -----------------------
## -----------------------

## -----------------------
## -----------------------



# Load the raw training data and replace missing values with NA
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

# Output the number of missing values for each column
sapply(training.data.raw,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(training.data.raw, function(x) length(unique(x)))

# A visual way to check for missing data
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

# Subsetting the data
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

# Substitute the missing values with the average value
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# R should automatically code Embarked as a factor(). A factor is R's way of dealing with
# categorical variables
is.factor(data$Sex)         # Returns TRUE
is.factor(data$Embarked)    # Returns TRUE

# Check categorical variables encoding for better understanding of the fitted model
contrasts(data$Sex)
contrasts(data$Embarked)

# Remove rows (Embarked) with NAs
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

# Train test splitting
train <- data[1:800,]
test <- data[801:889,]

# Model fitting
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

# Analysis of deviance
anova(model,test="Chisq")

# McFadden R^2
library(pscl)
pR2(model)

#-------------------------------------------------------------------------------
# MEASURING THE PREDICTIVE ABILITY OF THE MODEL

# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix
library(caret)
confusionMatrix(data=fitted.results, reference=test$Survived)

library(ROCR)
# ROC and AUC
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
# TPR = sensitivity, FPR=specificity
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc