#Removing all the objects from the R environment
rm(list = ls())

#Setting working directory
setwd("G:/Edwiser material/Project/buffer project")

# checking the set directory
getwd()

#Loading the required Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "unbalanced","dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'NbClust','fastDummies')

#installing the loaded packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#loading the data
df=read.csv("credit-card-data.csv")

#############Data preprocessing######################
#Removing the cust_ID coulmn as it is nothing nothing but ID
df$CUST_ID = NULL

#Checking for the missing values and imputing with knn imputation
#missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
#missing_val$Columns = row.names(missing_val)
#names(missing_val)[1] =  "Missing_percentage"
#missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100
#missing_val = missing_val[order(-missing_val$Missing_percentage),]
#row.names(missing_val) = NULL
#missing_val = missing_val[,c(2,1)]
#write.csv(missing_val, "Missing_perc.csv", row.names = F)

#class(df$CREDIT_LIMIT)
#class(df$MINIMUM_PAYMENTS)

#unique(df$CREDIT_LIMIT)
#unique(df$MINIMUM_PAYMENTS)

#There are missing values present in CREDIT_LIMIT & MINIMUM_PAYMENTS columns 
#Checking for the right method of missing value imputation 
#Real value =6250
#mean method imputation = 4494.253
#median method impuation = 3000
#knn impuation = 5362.473

#df$CREDIT_LIMIT[71]
#df$CREDIT_LIMIT[71] = NA

# Mean Method
#mean(df$CREDIT_LIMIT, na.rm = T)

#Median Method
#median(df$CREDIT_LIMIT, na.rm = T)

# kNN Imputation
#df=knnImputation(df, k = 3)

#Real value = 2180.882
#mean value = 864.0541
#median value = 312.2556
#knn imputation = 2157.051

#df$MINIMUM_PAYMENTS[71]
#df$MINIMUM_PAYMENTS[71] = NA

# Mean Method
#mean(df$MINIMUM_PAYMENTS, na.rm = T)

#Median Method
#median(df$MINIMUM_PAYMENTS, na.rm = T)

# kNN Imputation
df=knnImputation(df, k = 3)

dim(df)
str(df)

unique(df$TENURE)
unique(df$PURCHASES_TRX)

#########Exploratory data Analysis#####################
#####Advanced data preparation
##Deriving new KPI of Monthly average Purchases
#MONTHLY avg purchases Derivation
df$MONTH_AVG_PURCHASES = df$PURCHASES/df$TENURE

#Monthly cash advance derivation
df$MONTHLY_CASH_ADVANCE = df$CASH_ADVANCE/df$TENURE

#Checking for the Purchases by type (one-off, instalments) number of both
#oneoff purchases and installment purchases which are greater than zero and are equal to zero 
nrow(df[which(df$ONEOFF_PURCHASES > 0 & df$INSTALLMENTS_PURCHASES > 0),])

#no of both oneoff purchases and installment purchases which are qual to zero
nrow(df[which(df$ONEOFF_PURCHASES==0 & df$INSTALLMENTS_PURCHASES==0),])

#no of oneoff purchases are zero and installment purchases are greater than zero
nrow(df[which(df$ONEOFF_PURCHASES == 0 & df$INSTALLMENTS_PURCHASES > 0),])

#no of one off purchases are higher than zero and installments are zero
nrow(df[which(df$ONEOFF_PURCHASES >0 & df$INSTALLMENTS_PURCHASES== 0),])

#With the above details it is clear that there four different types of transaction which are used for 
#deriving a new feature


#creating a definition for new features
df$PURCHASE_TYPE[df$ONEOFF_PURCHASES == 0 & df$INSTALLMENTS_PURCHASES ==0]= "NONE"
df$PURCHASE_TYPE[df$ONEOFF_PURCHASES >0 & df$INSTALLMENTS_PURCHASES > 0]= "ONEOFF_INSTALLMENT"
df$PURCHASE_TYPE[df$ONEOFF_PURCHASES >0 & df$INSTALLMENTS_PURCHASES == 0]= "ONEOFF"
df$PURCHASE_TYPE[df$ONEOFF_PURCHASES ==0 & df$INSTALLMENTS_PURCHASES >0]= "INSTALLMENT"

# Limit usage calculation from balance to credit ratio
df$limit_usage = df$BALANCE/df$CREDIT_LIMIT

###Payments to minimum payments ratio
#Payments to minimum payments ratio calculation
df$Payment_minpay_Ratio = df$PAYMENTS/df$MINIMUM_PAYMENTS

#Separating PURCHASE_TYPE varibale from the data to convert the all the data into log
#transformation so that outliers can be removed
df2=data.frame(df$PURCHASE_TYPE)
df$PURCHASE_TYPE=NULL
df=log(df+1)

dim(df)
sum(is.na(df))

############Feature Selection########################
#Extracting numeric index of the data to check the Correlation 
numeric_index = sapply(df,is.numeric) #selecting only numeric
numeric_data = df[,numeric_index]
cnames = colnames(numeric_data)

##Correlation Plot 
corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

####################### Dimension Reduction##########################
#Removing the highly positive and negatively correlated variables
df = subset(df, select = -c(BALANCE,CASH_ADVANCE,PURCHASES_FREQUENCY,
                            CASH_ADVANCE_FREQUENCY,TENURE))
##############################Feature Scaling##############################
#Normality check
hist(df$BALANCE_FREQUENCY)

#Extracting the column names 
cnames=colnames(df)

#Normalisation
for(i in cnames){
#  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i] - min(df[,i])))
}

#Renaming the column name
names(df2)[1]="PURCHASE_TYPE"

#Getting dummies for each category of the column
df2=dummy_columns(df2$PURCHASE_TYPE)

#Removing the variable used for extraction of dummy variables  
df2$.data=NULL

#Joining the dummy varibale extracted data to the main data
d= cbind(df, df2)

#############Implementation of machine learing model#########
#Extracting the number of clusters to be bulid
NBclust_res = NbClust(d, min.nc=2, max.nc=15, method = "kmeans")

#Ploting a Barplot to analyse the optimum clusters
barplot(table(NBclust_res$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#Applying the K-mean clustering with Four no of clusters
kmeans_model = kmeans(d, 4, nstart=25)

#Summarizing the clustering output
kmeans_model
