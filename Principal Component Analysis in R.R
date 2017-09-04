                          #Principal Component Analysis (Dimensionality Reduction Technique)
                                  #Sibi Senthur Muthusamy
test<-read.csv(file.choose()) #Load the test data
dim(test)
#5681 rows 12 columns

train<-read.csv(file.choose())#Load the train data
dim(train) #8523 rows 12 columns 

View(test)
View(train)

#Adding a column
test$Item_Outlet_Sales <-1
#combine the data set
combi <- rbind(train, test)
#impute missing values with median
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
#impute 0 with median
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),combi$Item_Visibility)

#find mode and impute
table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)[1]<-"Other"

#Till here, we've imputed missing values. 
#Now we are left with removing the dependent (response) variable and other identifier variables( if any). 
#As we said above, we are practicing an unsupervised learning technique, hence response variable must be removed.

#remove the dependent and identifier variables

my_data <- subset(combi, select = -c(Item_Outlet_Sales, Item_Identifier,Outlet_Identifier)) 
#subsetting the combined dataframe so that we take only the independent variables

#Checking available variables
colnames(my_data)
#Checking variable class
str(my_data)

#Principal component analysis works only with numeric variables, so we are converting
#the existing the categorical variables into numeric !
#One-hot Encoding

#Creating a dummy data frame 
install.packages("dummies")
library(dummies)
new_my_data <- dummy.data.frame (my_data, names = c("Item_Fat_Content","Item_Type",
                                                    "Outlet_Establishment_Year","Outlet_Size",
                                                    "Outlet_Location_Type","Outlet_Type"))

#Check the data set!
str(new_my_data)

#We now all the numerical values. Diving the data into test and train 
pca.train <- new_my_data[1:nrow(train),]
pca.test <-new_my_data[-(1:nrow(train)),]

#Principal Component Analysis
prin_comp <-prcomp(pca.train,scale.=T)
#The base R function prcomp() is used to perform PCA. By default, it centers the variable to have mean equals to zero. With parameter scale. = T, we normalize the variables to have standard deviation equals to 1.
names(prin_comp)

#Output the mean of variables
prin_comp$center

#outputs the standard deviation of variables 
prin_comp$scale

#The rotation measure provides the principal component loading. 
#Each column of rotation matrix contains the principal component loading vector. 
#This is the most important measure we should be interested in.
prin_comp$rotation[1:5,1:4]

dim(prin_comp$x)
biplot(prin_comp, scale = 0)

#Compute standard deviation of each principal component 
std_dev <-prin_comp$sdev

#Compute Variance 
pr_var<-std_dev^2

#Check variance of first 10 components 
pr_var[1:10]
#We aim to find the components which explain the maximum variance. 
#We want to retain as much information as possible using these components

#Proportion of variance explained 
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#Scree Plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#The plot above shows that ~ 30 components explains around 98.4% variance in the data set. In order words, using PCA we have reduced 44 predictors to 30 without compromising on explained variance. 
#This is the power of PCA> Let's do a confirmation check, by plotting a cumulative variance plot. 
#This will give us a clear picture of number of components.

#Cumulative Screen PLot (Will give a clear picture on how many variables are exactly needed)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Adding a training set with principal components 
train.data<-data.frame(Item_Outlet_Sales=train$Item_Outlet_Sales, prin_comp$x)
View(train.data)

#We are interested in first 30 PCAs
train.data<-train.data[,1:31]

#Run a Decision Tree
install.packages("rpart")
library(rpart)
rpart.model <-rpart(Item_Outlet_Sales~.,data=train.data,method="anova")
rpart.model

#transform test into PCA
test.data<-predict(prin_comp,newdata=pca.test)
test.data<-as.data.frame(test.data)

#Select the first 30 Components
test.data<-test.data[,1:30]

#make predictions on the test data 
rpart.prediction<-predict(rpart.model,test.data)

final.sub <-data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
sample<-read.csv(file.choose())
View(sample)
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
View(final.sub)
