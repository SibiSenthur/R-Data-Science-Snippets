                        # IDS 575 Advanced Statistics Final Project Design #
                    #Project Title: Predictive Analysis of Real Estate House Prices 

#Setting up the training and testing data
train_data<-read.csv("C:/Users/sakri18/Downloads/train (2).csv", header = T, stringsAsFactors = FALSE)
test_data<-read.csv("C:/Users/sakri18/Downloads/test (2).csv", header = T, stringsAsFactors = FALSE)
dim(train_data) #1460 rows 81 columns
dim(test_data) #1459 rows 80 columns
#We are trying to predict the dependent variable sale_price of house, which is excluded from the test data set.

combine_dataframe <- rbind(within(train_data, rm('Id','SalePrice')), within(test_data, rm('Id')))

dim(combine_dataframe) #2919 rows 79 columns
#--------------------------------------------------------------------------------------------------------------------------------
#FEATURE ENGINEERING (PART 1) -> Handling the Missing (N/A) values

#Finding if there are any missing values
na.columns <-which(colSums(is.na(combine_dataframe))>0)
sort(colSums(sapply(combine_dataframe[na.columns],is.na)), decreasing = TRUE)

#We observe that there are 2909 missing values for PoolQC, the highest among the missing value variables. We suspect that the NA's for 
#Pool Quality are for houses that don't have a swimming pool. Hence, we can further analyse if there are any Missing values for houses with pools (ie. by examining poolArea) for which PoolQC is NA

paste('There are',length(na.columns),'columns with missing values')

install.packages("ggplot2")
library(ggplot2)
#_______________________________________________________________________________________________________________________________
#Driver function to plot the categorical variable
plotting.categorical_variable <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(combine_dataframe[,col]), decreasing = TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}
str(combine_dataframe)
#-----------------------------------------------------------------------------------------------------------------------------
plotting.categorical_variable('PoolQC',combine_dataframe) #We observe that there are a total of 10 categorical values for the field PoolQC
#We need to examine if any of those ten variables are associated with Missing variables
check=combine_dataframe[(combine_dataframe$PoolArea>0)& is.na(combine_dataframe$PoolQC),c('PoolArea','PoolQC')]
check 
#So, we essentially have three observations where Pool Quality is NA and have values defined for Poolarea.
#We would essentially like to replace these three NA values with mean of the Pool Area depending on the category they belong to.

install.packages("dplyr")
library(dplyr)
combine_dataframe[,c('PoolQC','PoolArea')]%>% group_by(PoolQC) %>% summarise(mean=mean(PoolArea), counts=n()) 

#We Infer that there are 10 pool count and the pool quality has been assigned accordingly. However, we need to asssign the three NA's which
# had pool area values - a factor which depends on any of the mean values
combine_dataframe[2421,'PoolQC']='Ex'
combine_dataframe[2504,'PoolQC']='Ex'
combine_dataframe[2600, 'PoolQC']='Fa'
combine_dataframe$PoolQC[is.na(combine_dataframe$PoolQC)] = 'None'
#So, essentially we have repalced most of the NA values with the character variable 'None' with the exception of three values where we have imputed with mean values.

sort(colSums(sapply(combine_dataframe[na.columns],is.na)), decreasing = TRUE)
#Checking the Na values count we realise that we have handled the Na values for PoolQc !

#Now, we try to check if there is any correlation in the year house was built and the same year the garage was built..
str(combine_dataframe)
combine_dataframe$YearBuilt=as.numeric(combine_dataframe$YearBuilt)
combine_dataframe$GarageYrBlt=as.numeric(combine_dataframe$GarageYrBlt)
str(combine_dataframe)
length(which(combine_dataframe$GarageYrBlt == combine_dataframe$YearBuilt))
#Generally there  is a strong co-relation between the year the house was built and the year the garage was built as there 
#2216 of the 2919 houses which have their garage built in the same year as the house.

# the logic here is we are first retriving the index where the field values for garage year built is zer0
index<-which(is.na(combine_dataframe$GarageYrBlt))
head(index, n=25)

# We are essentially replacing those N/A with the copy of the same values in the Garage YearBuilt column.

combine_dataframe[index,'GarageYrBlt']<-combine_dataframe[index,'YearBuilt']
#We are essentially replacing the NA values in the garage built year field with the values from House built by passing the NA valued index.

garage.columns <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')
garage.columns
a1=combine_dataframe[is.na(combine_dataframe$GarageCond),(garage.columns)]
combine_dataframe[is.na(combine_dataframe$GarageCond),garage.columns]
idx <- which(((combine_dataframe$GarageArea < 370) & (combine_dataframe$GarageArea > 350)) & (combine_dataframe$GarageCars == 1))
idx
names(sapply(combine_dataframe[idx, garage.columns], function(x) sort(table(x), decreasing=TRUE)))
combine_dataframe[2127,'GarageQual'] = 'TA'
combine_dataframe[2127, 'GarageFinish'] = 'Unf'
combine_dataframe[2127, 'GarageCond'] = 'TA'

for (col in garage.columns){
  if (sapply(combine_dataframe[col], is.numeric) == TRUE){
    combine_dataframe[sapply(combine_dataframe[col], is.na), col] = 0
  }
  else{
    combine_dataframe[sapply(combine_dataframe[col], is.na), col] = 'None'
  }
}

library(ggplot2)
install.packages("scales")
#Kitchen Quality
plotting.categorical_variable('KitchenQual', combine_dataframe)
combine_dataframe$KitchenQual[is.na(combine_dataframe$KitchenQual)]='TA'
plotting.categorical_variable('Electrical', combine_dataframe)

install.packages("stringr")
library(stringr)
combine_dataframe$Electrical[is.na(combine_dataframe$Electrical)] = 'SBrkr'

bsmt.cols <- names(combine_dataframe)[sapply(names(combine_dataframe), function(x) str_detect(x, 'Bsmt'))]
bsmt.cols
combine_dataframe[is.na(combine_dataframe$BsmtExposure),bsmt.cols]
plotting.categorical_variable('BsmtExposure',combine_dataframe)

combine_dataframe[c(949, 1488, 2349), 'BsmtExposure'] = 'No'

for (col in bsmt.cols){
  if (sapply(combine_dataframe[col], is.numeric) == TRUE){
    combine_dataframe[sapply(combine_dataframe[col], is.na),col] = 0
  }
  else{
    combine_dataframe[sapply(combine_dataframe[col],is.na),col] = 'None'
  }
}

idx <- which(is.na(combine_dataframe$Exterior1st) | is.na(combine_dataframe$Exterior2nd))
idx
combine_dataframe[idx,c('Exterior1st', 'Exterior2nd')]

combine_dataframe$Exterior1st[is.na(combine_dataframe$Exterior1st)] = 'Other'
combine_dataframe$Exterior2nd[is.na(combine_dataframe$Exterior2nd)] = 'Other'

plotting.categorical_variable('SaleType', combine_dataframe)

table(combine_dataframe$SaleCondition, combine_dataframe$SaleType)
combine_dataframe$SaleType[is.na(combine_dataframe$SaleType)] = 'WD'

plotting.categorical_variable('Functional', combine_dataframe)

combine_dataframe$Functional[is.na(combine_dataframe$Functional)] = 'Typ'
plotting.categorical_variable('Utilities', combine_dataframe)

which(combine_dataframe$Utilities == 'NoSeWa') # in the training data set

col.drops <- c('Utilities')
combine_dataframe <- combine_dataframe[,!names(combine_dataframe) %in% c('Utilities')]

combine_dataframe[is.na(combine_dataframe$MSZoning),c('MSZoning','MSSubClass')]
plotting.categorical_variable('MSZoning',combine_dataframe)
table(combine_dataframe$MSZoning, combine_dataframe$MSSubClass)
combine_dataframe$MSZoning[c(2217, 2905)] = 'RL'
combine_dataframe$MSZoning[c(1916, 2251)] = 'RM'
combine_dataframe[(is.na(combine_dataframe$MasVnrType)) | (is.na(combine_dataframe$MasVnrArea)), c('MasVnrType', 'MasVnrArea')]
na.omit(combine_dataframe[,c('MasVnrType','MasVnrArea')]) %>%
  group_by(na.omit(MasVnrType)) %>%
  summarise(MedianArea = median(MasVnrArea,na.rm = TRUE), counts = n()) %>%
  arrange(MedianArea)

combine_dataframe[2611, 'MasVnrType'] = 'BrkFace'

combine_dataframe$MasVnrType[is.na(combine_dataframe$MasVnrType)] = 'None'
combine_dataframe$MasVnrArea[is.na(combine_dataframe$MasVnrArea)] = 0

combine_dataframe['Nbrh.factor'] <- factor(combine_dataframe$Neighborhood, levels = unique(combine_dataframe$Neighborhood))

lot.by.nbrh <- combine_dataframe[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm = TRUE))
lot.by.nbrh

idx = which(is.na(combine_dataframe$LotFrontage))

for (i in idx){
  lot.median <- lot.by.nbrh[lot.by.nbrh == combine_dataframe$Neighborhood[i],'median']
  combine_dataframe[i,'LotFrontage'] <- lot.median[[1]]
}

plotting.categorical_variable('Fence', combine_dataframe)

combine_dataframe$Fence[is.na(combine_dataframe$Fence)] = 'None'
table(combine_dataframe$MiscFeature)
combine_dataframe$MiscFeature[is.na(combine_dataframe$MiscFeature)] = 'None'
plotting.categorical_variable('FireplaceQu', combine_dataframe)

which((combine_dataframe$Fireplaces > 0) & (is.na(combine_dataframe$FireplaceQu)))
combine_dataframe$FireplaceQu[is.na(combine_dataframe$FireplaceQu)] = 'None'
plotting.categorical_variable('Alley', combine_dataframe)
combine_dataframe$Alley[is.na(combine_dataframe$Alley)] = 'None'
paste('There are', sum(sapply(combine_dataframe, is.na)), 'missing values left')
#--------------------------------------------------------------------------------------------------------------------
#Adding Customer Features 

#Creating a numeric dataframe 
num_features<-names(which(sapply(combine_dataframe, is.numeric)))
length(num_features)
#We observe that there are 36 numeric features
#Creating a character data type only dataframe
cat_features<-names(which(sapply(combine_dataframe, is.character)))
df.numeric<-combine_dataframe[num_features]
#Extracting the numeric only data frame

group.df<-combine_dataframe[1:1460,] #Appending the traind data
group.df$SalePrice<-train_data$SalePrice #Appending the target variable

#Now Lets start with the One-Hot encoding

group.prices <- function(col) {
  group.table <- group.df[,c(col,'SalePrice','OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.Quality = round(mean(OverallQual),2),
              mean.Price = mean(SalePrice), n = n()) %>%
    arrange(mean.Quality)
  return(data.frame(group.table))
}

quality.mean <- function(col) {
  group.table <- combine_dataframe[,c(col, 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.qual = mean(OverallQual)) %>%
    arrange(mean.qual)
  
  return(data.frame(group.table))
}


map.fcn <- function(cols, map.list, df){
  for (col in cols){
    df[col] <- as.numeric(map.list[combine_dataframe[,col]])
  }
  return(df)
}


install.packages("psych")
library(psych)
qual.cols <- c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC', 'BsmtQual')
group.prices('FireplaceQu')
group.prices('KitchenQual')

qual.list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
df.numeric <- map.fcn(qual.cols, qual.list, df.numeric)


combine_dataframe[,c('BsmtFinType1', 'BsmtFinSF1')] %>%
  group_by(BsmtFinType1) %>%
  summarise(medianArea = median(BsmtFinSF1), counts = n()) %>%
  arrange(medianArea) 
  
bsmt.fin.list <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
df.numeric<-map.fcn(c('BsmtFinType1','BsmtFinType2'), bsmt.fin.list,df.numeric)


group.prices('Functional')
functional.list <- c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4, 'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)
df.numeric<-map.fcn(c('Functional'), functional.list,df.numeric)
str(df.numeric)

group.prices('GarageFinish')
garage.fin.list <-c(None='0',Unf='1',RFn='1',Fin='2')
df.numeric<-map.fcn(c('GarageFinish'),garage.fin.list,df.numeric)
str(df.numeric)
df.numeric$GarageFinish

group.prices('Fence')
fence.list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)
df.numeric<-map.fcn(c('Fence'),fence.list,df.numeric)
str(df.numeric)

MSdwelling.list <- c('20' = 1, '30'= 0, '40' = 0, '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0)
df.numeric['NewerDwelling'] <- as.numeric(MSdwelling.list[as.character(combine_dataframe$MSSubClass)])

#We are done with the one-hot encoding for our character features !

#Since, we have done the necessary transformation, we will see if we can do the co-relation of the 
#resulting numeric variables and the targer variable salesprice

corr.df<-cbind(df.numeric[1:1460,],train_data['SalePrice'])
#Now I am combining the numeric features and the target variable salesprice
correlations<-cor(corr.df)
corr.SalePrice<-as.matrix(sort(correlations[,'SalePrice']), decreasing=TRUE)
corr.SalePrice

corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))
corr.idx
install.packages("corrplot")
library(corrplot)
corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)

corr.SalePrice<-as.matrix(sort(correlations[,'SalePrice']), decreasing=TRUE)

install.packages("GGally")
library("GGally")

lm.plt <- function(data, mapping, ...){
  plt <- ggplot(data = data, mapping = mapping) + 
    geom_point(shape = 20, alpha = 0.7, color = 'darkseagreen') +
    geom_smooth(method=loess, fill="red", color="red") +
    geom_smooth(method=lm, fill="blue", color="blue") +
    theme_minimal()
  return(plt)
}

ggpairs(corr.df, corr.idx[11:16], lower = list(continuous = lm.plt))
plotting.categorical_variable('LotShape',combine_dataframe)
df.numeric['RegularLotShape'] <- (combine_dataframe$LotShape == 'Reg') * 1
plotting.categorical_variable('LandContour',combine_dataframe)
df.numeric['LandLeveled'] <- (combine_dataframe$LandContour == 'Lvl') * 1

plotting.categorical_variable('LandSlope', combine_dataframe)
df.numeric['LandSlopeGentle'] <- (combine_dataframe$LandSlope == 'Gtl') * 1
plotting.categorical_variable('Electrical', combine_dataframe)
df.numeric['ElectricalSB'] <- (combine_dataframe$Electrical == 'SBrkr') * 1

plotting.categorical_variable('GarageType', combine_dataframe)
df.numeric['GarageDetchd'] <- (combine_dataframe$GarageType == 'Detchd') * 1
plotting.categorical_variable('PavedDrive', combine_dataframe)

df.numeric['HasWoodDeck'] <- (combine_dataframe$WoodDeckSF > 0) * 1

df.numeric['Has2ndFlr'] <- (combine_dataframe$X2ndFlrSF > 0) * 1

df.numeric['HasMasVnr'] <- (combine_dataframe$MasVnrArea > 0) * 1
df.numeric['HasShed'] <- (combine_dataframe$MiscFeature == 'Shed') * 1

df.numeric['Remodeled'] <- (combine_dataframe$YearBuilt != combine_dataframe$YearRemodAdd) * 1
df.numeric['RecentRemodel'] <- (combine_dataframe$YearRemodAdd >= combine_dataframe$YrSold) * 1

df.numeric['NewHouse'] <- (combine_dataframe$YearBuilt == combine_dataframe$YrSold) * 1

cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')

combine_dataframe$X2ndFlrSF
combine_dataframe$MasVnrArea
combine_dataframe$WoodDeckSF

cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')

for (col in cols.binary){
  df.numeric[str_c('Has',col)] <- (combine_dataframe[,col] != 0) * 1
}

ggplot(combine_dataframe, aes(x=MoSold)) +
  geom_bar(fill = 'cornflowerblue') +
  geom_text(aes(label=..count..), stat='count', vjust = -.5) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12)

combine_dataframe$MoSold
df.numeric['HighSeason']<-(combine_dataframe$MoSold%in%c(5,6,7))*1

train_data[,c('Neighborhood','SalePrice')] %>%
  group_by(Neighborhood) %>%
  summarise(median.price = median(SalePrice, na.rm = TRUE)) %>%
  arrange(median.price) %>%
  mutate(nhbr.sorted = factor(Neighborhood, levels=Neighborhood)) %>%
  ggplot(aes(x=nhbr.sorted, y=median.price)) +
  geom_point() +
  geom_text(aes(label = median.price, angle = 45), vjust = 2) +
  theme_minimal() +
  labs(x='Neighborhood', y='Median price') +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45))


other.nbrh <- unique(combine_dataframe$Neighborhood)[!unique(combine_dataframe$Neighborhood) %in% c('StoneBr', 'NoRidge','NridgHt')]
nbrh.rich <- c('Crawfor', 'Somerst, Timber', 'StoneBr', 'NoRidge', 'NridgeHt')
df.numeric['NbrhRich'] <- (combine_dataframe$Neighborhood %in% nbrh.rich) *1

group.prices('Neighborhood')
nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
              'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
              'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
              'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
              'NridgHt' = 4)

group.prices('SaleCondition')
df.numeric['PartialPlan'] <- (combine_dataframe$SaleCondition == 'Partial') * 1
group.prices('HeatingQC')

heating.list <- c('Po' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)
df.numeric['HeatingScale'] <- as.numeric(heating.list[combine_dataframe$HeatingQC])
area.cols <- c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
               'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
               'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')

df.numeric['TotalArea'] <- as.numeric(rowSums(combine_dataframe[,area.cols]))
df.numeric['AreaInside'] <- as.numeric(combine_dataframe$X1stFlrSF + combine_dataframe$X2ndFlrSF)
df.numeric['Age'] <- as.numeric(2010 - combine_dataframe$YearBuilt)
df.numeric['TimeSinceSold'] <- as.numeric(2010 - combine_dataframe$YrSold)
df.numeric['YearSinceRemodel'] <- as.numeric(combine_dataframe$YrSold - combine_dataframe$YearRemodAdd)
corr.OverallQual <- as.matrix(sort(correlations[,'OverallQual'], decreasing = TRUE))
corr.idx <- names(which(apply(corr.OverallQual, 1, function(x) (x > 0.5 | x < -0.5))))
corrplot(as.matrix(correlations[corr.idx, corr.idx]), type = 'upper',
         method = 'color', addCoef.col = 'black', tl.cex =.7, cl.cex = .7,
         number.cex = .7)


dim(df.numeric)
str(df.numeric)
train.test.df <- rbind(dplyr::select(train_data,-SalePrice), test_data)
train.test.df$type <- c(rep('train_data',1460),rep('test_data',1459))
ggplot(train_data, aes(x=GrLivArea)) +
  geom_histogram(fill='lightblue',color='white') +
  theme_minimal()
#____________________________________________________________________________________________________________________________
#Outlier Values Detection

outlier_values <- boxplot.stats(train_data$GrLivArea)$out  # outlier values.
boxplot(train_data$GrLivArea, main="GrLivArea", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values[outlier_values>4000], collapse=", ")), cex=0.6)

idx.outliers <- which(train_data$GrLivArea > 4000)
idx.outliers
dim(df.numeric)
df.numeric <- df.numeric[!1:nrow(df.numeric) %in% idx.outliers,]
dim(df.numeric)
combine_dataframe <- combine_dataframe[!1:nrow(combine_dataframe) %in% idx.outliers,]

require(psych)
install.packages('moments')
library('moments')
skewed <- apply(df.numeric, 2, skewness)
skewed <- skewed[(skewed > 0.8) | (skewed < -0.8)]

kurtosis <- apply(df.numeric, 2, kurtosi)
kurtosis <- kurtosis[(kurtosis > 3.0) | (kurtosis < -3.0)]


for(col in names(skewed)){
  if(0 %in% df.numeric[, col]) {
    df.numeric[,col] <- log(1+df.numeric[,col])
  }
  else {
    df.numeric[,col] <- log(df.numeric[,col])
  }
}
install.packages('caret')
library('caret')
scaler <- preProcess(df.numeric)
scaler
df.numeric <- predict(scaler, df.numeric)
df.numeric

#For the rest of the categoric features we can one-hot encode each value to get as many splits in the data as possible

dummy <- dummyVars(" ~ .",data=combine_dataframe[,cat_features])
df.categoric <- data.frame(predict(dummy,newdata=combine_dataframe[,cat_features]))
View(df.categoric)

year.map = function(col.combined, col.name) {
  for (i in 1:7) {
    year.seq = seq(1871+(i-1)*20, 1871+i*20-1)
    idx = which(combine_dataframe[,col.combined] %in% year.seq)
    df.categoric[idx,col.name] = i
  }
  return(df.categoric)
}

df.categoric['GarageYrBltBin'] = 0
df.categoric <- year.map('GarageYrBlt', 'GarageYrBltBin')
df.categoric['YearBuiltBin'] = 0
df.categoric <- year.map('YearBuilt','YearBuiltBin')
df.categoric['YearRemodAddBin'] = 0
df.categoric <- year.map('YearRemodAdd', 'YearRemodAddBin')

bin.cols <- c('GarageYrBltBin', 'YearBuiltBin', 'YearRemodAddBin')

for (col in bin.cols) {
  df.categoric <- cbind(df.categoric, model.matrix(~.-1, df.categoric[col]))
}

# lets drop the orginal 'GarageYrBltBin', 'YearBuiltBin', 'YearRemodAddBin' from our dataframe
df.categoric <- df.categoric[,!names(df.categoric) %in% bin.cols]

df <- cbind(df.numeric, df.categoric)
View(df)
dim(df)

require(WVPlots)
y.true <- train_data$SalePrice[which(!1:1460 %in% idx.outliers)]
y_train <- log(y.true+1)


qqnorm(train_data$SalePrice)
qqline(train_data$SalePrice)

qqnorm(y_train)
qqline(y_train)
paste('The dataframe has', dim(df)[1], 'rows and', dim(df)[2], 'columns')

nzv.data <- nearZeroVar(df, saveMetrics = TRUE) #Eliminating near zero variance
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]
df <- df[,!names(df) %in% drop.cols]
paste('The dataframe now has', dim(df)[1], 'rows and', dim(df)[2], 'columns')

View(df)

x_train <- df[1:1456,]
View(x_train)

x_test <- df[1457:nrow(df),]

dim(df)
x_train1 <- df[1:1456,]
dim(x_train1)
x_test1 <- df[1457:nrow(df),]
dim(x_test1)
y_train1 <- log(y.true+1)

#Testing out the Ridge Regression Model
#_______________________________________________________________________________________________________________________________
caret.train.ctrl<-trainControl(method="repeatedcv", number=5,repeats = 5, verboseIter = FALSE)
lambdas <- seq(1,0,-0.001)
set.seed(123)
model_ridge <- train(x=x_train1,y=y_train1,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=caret.train.ctrl,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))

mean(model_ridge$resample$RMSE) #0.06335041
ggplot(data=filter(model_ridge$result,RMSE<0.14)) +
  geom_line(aes(x=lambda,y=RMSE))
#______________________________________________________________________________________________________________________________

# Testing out the Least Absolute Shrinkage Model and Selection Operator Model (Lasso) regression model
set.seed(123) 
model_lasso <- train(x=x_train1,y=y_train1,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=caret.train.ctrl,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
mean(model_lasso$resample$RMSE) #0.01153741
#___________________________________________________________________________________________________________________________

sample_size<-floor(0.75*nrow(x_train1))
sample_size
x_train1$SalePrice<-y_train1
#Setting the seed to make the partition reproducable !
set.seed(123)
train_ind<-sample(seq_len(nrow(x_train1)), size=sample_size)
train_ind
train_new<-x_train1[train_ind,]
validate<-x_train1[-train_ind,]

library(randomForest)
forest<-randomForest(SalePrice~.,train_new)
varImpPlot(forest)
prediction1<-predict(forest,validate)
rmse<-sqrt(mean(prediction1-validate$SalePrice)^2)
rmse #0.0008199741

submit<-data.frame(SalePrice=prediction)
write.csv(submit,file="randomF.csv")
View(x_train1)
View(validate)
View(x_train1$SalePrice)

View(validate$SalePrice)

a=(prediction[1] - validate$SalePrice[1])
a
#____________________________________________________________________________________________________________________________
#Testing out the General Linear Model Regression
fit2<-lm(SalePrice~.,train_new)
fit<-lm(SalePrice~.,x_train1)
plot(fit$fitted.values,fit$residuals)

prediction<-predict(fit2,validate,type="response")
summary(fit2)
length(validate$SalePrice)
 length(prediction)
is.na(x_train1)

mse<-sqrt(mean(prediction-validate$SalePrice)^2)
mse #0.0008240056

prediction[1]
#12.1942
validate$SalePrice[1]
#12.10902
#________________________________________________________________________________________________________________________________


