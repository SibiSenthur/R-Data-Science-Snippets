                                            # Code For handling Class imbalance 

                                              #Sibi Senthur Muthusamy (UIN 655532606)
#SMOTE (Synthetic Minority Over-sampling Technique) #SuperSampling Rare Events in R

#Pacakges Used  -> {#DMwR, #caret, #pROC}

#I am using the SMOTE function which oversamples rare event 
#using bootstraping and k-nearest neighbour to synthetically create additional observations of that event !

#------------------ <Preparing the data>
hyper <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data', header=F)
#This dataset is an extract froM UCI Machine Learning Repository
View(hyper)
names <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', header=F, sep='\t')[[1]]
View(names)
names <- gsub(pattern =":|[.]",x = names, replacement="")
View(names)
colnames(hyper)<-names
View(hyper)
#--------------------------------------------------------

#I change the target column name from hypothyroid, negative. to target and set any values of negative to 0 and everything else to 1:
  
colnames(hyper) <-c("target", "age", "sex", "on_thyroxine", "query_on_thyroxine",
                    "on_antithyroid_medication", "thyroid_surgery", "query_hypothyroid",
                    "query_hyperthyroid", "pregnant", "sick", "tumor", "lithium",
                    "goitre", "TSH_measured", "TSH", "T3_measured", "T3", "TT4_measured",
                    "TT4", "T4U_measured", "T4U", "FTI_measured", "FTI", "TBG_measured",
                    "TBG")
hyper$target <- ifelse(hyper$target=='negative',0,1)
print(table(hyper$target))
print(prop.table(table(hyper$target))) #At 5%, we conculed that this is clearly a skewed dataset !

head(hyper,2)
ind<-sapply(hyper,is.factor)
ind
str(hyper)

hyper[ind]<-lapply(hyper[ind],as.character)

hyper[ hyper == "?" ] = NA
hyper[ hyper == "f" ] = 0
hyper[ hyper == "t" ] = 1
hyper[ hyper == "n" ] = 0
hyper[ hyper == "y" ] = 1
hyper[ hyper == "M" ] = 0
hyper[ hyper == "F" ] = 1

hyper[ind] <- lapply(hyper[ind], as.numeric)

#Replace 'NA' values with mean!
repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
hyper <- repalceNAsWithMean(hyper)

#Reference model
#Splitting the data set into training and test data sets

install.packages("caret")
library(caret)
set.seed(1234)

splitIndex <-createDataPartition(hyper$target,p=.50,list=FALSE, times = 1)
trainsplit1<-hyper[splitIndex,] #Splitting the data frame into training and test data
testsplit1<-hyper[-splitIndex,]

prop.table(table(trainsplit1$target)) # 0 -> 0.95006321 1-> 0.04993679 
prop.table(table(testsplit1$target)) # 0 -> 0.9544592 1->0.0455408

#we train a treebag model using caret syntax on trainSplit and predict hyperthyroidism on the testSplit portion

ctrl <-trainControl(method = "cv",number = 5)
tbmodel <- train(target ~ .,data=trainsplit1,method="treebag",trControl=ctrl)
#tbmodel$finalModel

predictors <- names(trainsplit1)[names(trainsplit1)!='target']
pred<-predict(tbmodel$finalModel,testsplit1[,predictors])

#Evaluating the model with the package pROC for an auc score and plot

install.packages("pROC")
library(pROC)
auc<-roc(testsplit1$target,pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

#An auc score of 0.98 is great (remember it ranges on a scale between 0.5 and 1, where 0.5 is random and 1 is perfect). 
#It is hard to imagine that SMOTE can improve on this, but. 

#Let's SMOTE

#Let's create extra positive observations using SMOTE. 
#We set perc.over = 100 to double the quantity of positive cases, and set perc.under=200 to keep half of what was created as negative cases.
install.packages("DMwR")
library(DMwR)
trainsplit1$target <- as.factor(trainsplit1$target)
trainsplit1 <- SMOTE(target ~ ., trainsplit1, perc.over = 100, perc.under=200)
trainsplit1$target <- as.numeric(trainsplit1$target)

#We can check the outcome balance with prop.table and confirm that we equalized the data set between positive and negative cases of hyperthyroidism.
prop.table(table(trainsplit1$target)) #We observe a 50-50 split

tbmodel <- train(target ~ ., data = trainsplit1, method = "treebag",
                 trControl = ctrl)
predictors <- names(trainsplit1)[names(trainsplit1) != 'target']
pred <- predict(tbmodel$finalModel, testsplit1[,predictors])

auc <- roc(testsplit1$target, pred)
print(auc) #0.991, it managed to better the old auc score of 0.985!

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

#we ended up reducing the overall size and getting a better score. 
#SMOTE works great in some situation and not-so-great in others. This definitely requires some trial-and-error but the concept is very promising when stuck with extremely skewed and, therefore, overly sensitive data.


