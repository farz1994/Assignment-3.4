1. Import the Titanic Dataset from the link Titanic Data Set.
Perform the following:

a. Preprocess the passenger names to come up with a list of titles that represent families
and represent using appropriate visualization graph.
library(readxl)
titanic_dataset <- read_xls(file.choose())
View(titanic_dataset)
str(titanic_dataset)
strsplit(titanic_dataset$name[5], split = '[,.]')[[1]][2]
titanic_dataset$Title <- sapply(titanic_dataset$name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic_dataset$Title <- sub(' ', '', titanic_dataset$Title)
table(titanic_dataset$Title)
titanic_dataset$Title[titanic_dataset$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
titanic_dataset$Title[titanic_dataset$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
titanic_dataset$Title[titanic_dataset$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(titanic_dataset$Title)
barplot(table(titanic_dataset$Title),main = "survival as per title", xlab = "Title", ylab = "count",col ="red")

b. Represent the proportion of people survived from the family size using a graph.
titanic_dataset$familysize <- titanic_dataset$sibsp+titanic_dataset$parch+1
library(ggplot2)
ggplot(titanic_dataset, aes(x = familysize, fill = factor(survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') 


c. Impute the missing values in Age variable using Mice Library, create two different
graphs showing Age distribution before and after imputation.

library(mice)
titanic_dataset$age
sum(is.na(titanic_dataset$age))
factor_vars <- c('pclass','sex','embarked', 'Title','familysize')
titanic_dataset[factor_vars] <- lapply(titanic_dataset[factor_vars], function(x) as.factor(x))
set.seed(123)
mice_mod <- mice(titanic_dataset[, !names(titanic_dataset) %in% c('pclass','sex','embarked','Title','familysize')], method='rf')
mice_output <- complete(mice_mod)
titanic_dataset$Newage <- mice_output$age
as.integer(titanic_dataset$Newage)
par(mfrow=c(1,2))
hist(titanic_dataset$age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(titanic_dataset$Newage, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

