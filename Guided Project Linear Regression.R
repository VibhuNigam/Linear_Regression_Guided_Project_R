#Steps: ( For training Models)
#1- Read/Collect your data
#2- Data Engineering
#3- Treat Missing / Outlier Values
#4- Train Test Split
#5- Run Model on Train and Predict on Test 70/30 , 80/20 split 
#6- Check accuracy Measures and select the best Model
#7- Run Model on Complete Dataset 100% 
#8- Do your final prediction ( 300 or so customers)


setwd("D:\\Edvancer\\R\\Data (2)")

### What are we Trying to Predict ? % Intrest Rates

ld_train=read.csv("loan_data_train.csv")

ld_test= read.csv("loan_data_test.csv")


ld_test$Interest.Rate=NA




ld_train$data='train'
ld_test$data='test'




ld_all=rbind(ld_train,ld_test)



library(dplyr)

glimpse(ld_all)

table(ld_all$Amount.Requested )






# STEPS :

#Convert to Numeric : AMount requested , Open.CREDIT.Lines , Revolving.CREDIT.Balance
#Remove Amount.Funded.By.Investors
#Remove % Symbol and then  Convert to Numeric Interest.Rate   , Debt.To.Income.Ratio 
#Remove Months and take numeric / Treat as Categorical And create dummy
#Create Dummies ( all Categorical Variables)
#Average of range Fico
#Categorical / Numeric We can try for both

#dbl,flt decimal values

table(ld_all$FICO.Range)


### Float and Double are numeric with Decimal Points 




# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL


library(dplyr)

glimpse(ld_all)


ld_all=ld_all %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )


# Warning your code has run 
# Error Means your code hasnt run

glimpse(ld_all)

ld_all=ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)

#### Try converting to numeric and see wheter the results are better ?

table(ld_all$Loan.Purpose)

xyz=as.data.frame(round(tapply(ld_all$Interest.Rate,ld_all$Loan.Purpose,mean,na.rm=T)))

xyz$colnames=row.names(xyz)


ld_all=ld_all %>% 
  mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
         lp_11=as.numeric(Loan.Purpose %in% c("major_purchase","medical","car")),
         lp_12=as.numeric(Loan.Purpose %in% c("vacation","wedding","home_improvement")),
         lp_13=as.numeric(Loan.Purpose %in% c("other","small_business","credit_card")),
         lp_14=as.numeric(Loan.Purpose %in% c("debt_consolidation","house","moving"))) %>% 
  select(-Loan.Purpose)

glimpse(ld_all)


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(ld_all)

table(ld_all$Home.Ownership)

ld_all=CreateDummies(ld_all ,"State",50)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)

glimpse(ld_all)

library(tidyr)


ld_all=ld_all %>% 
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)


glimpse(ld_all)

table(ld_all$Employment.Length)

ld_all=CreateDummies(ld_all,"Employment.Length",100)

## NA values

lapply(ld_all,function(x) sum(is.na(x)))

# Impute the values ( Large number of missing values )
# Drop the Rows ()


ld_all=ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all=ld_all[!(is.na(ld_all[,col])),]
    
  }
  
}


lapply(ld_all,function(x) sum(is.na(x)))

## separate train and test

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)



#### 
# Split ld_train into Train and Test for modelling


##

set.seed(2)
## SAMPLE(Vector , no of samples)
s=sample(1:nrow(ld_train),0.8*nrow(ld_train))
length(s)

ld_train1=ld_train[s,] ## Training dataset 80%
ld_train2=ld_train[-s,] ## Test dataset 20%


### Fit a model lm ( Linear model )

### Intrest rate - Dependent Variable 
### Variables used to Predict - Independent Variables 

#lm(dependent ~ Indepdent Variables , Dataset )


fit=lm(Interest.Rate~.-ID,data=ld_train1)

summary(fit)

library(car)

vif(fit)
# we'll take vif cutoff as 5

sort(vif(fit),decreasing = T)

fit=lm(Interest.Rate~.-ID,data=ld_train1)



sort(vif(fit),decreasing = T)

# p-value take the cutoff .05

summary(fit)

### Select the correct variables 
### Forward Selection / Backward Selection 
## AIC AND BIC SCORES : Depths / 


fit=step(fit) 

## AIC score 

summary(fit)

formula(fit)

fit=lm(Interest.Rate ~ Amount.Requested +Revolving.CREDIT.Balance+ Open.CREDIT.Lines + 
         Inquiries.in.the.Last.6.Months + ll_36  + State_TX + 
         Home.Ownership_MORTGAGE + fico,
       data=ld_train1)

summary(fit)

### predict ( model , data to predict on )

val.pred=predict(fit,newdata=ld_train2)

errors=ld_train2$Interest.Rate-val.pred

## MAE = Mean absolute Error 

MAE=sum(abs(errors))/nrow(ld_train2)

## RMSE Root mean square error 

RMSE=errors**2 %>% mean() %>% sqrt()



### model for predcition on the entire data

fit.final=fit=lm(Interest.Rate ~ Amount.Requested +Revolving.CREDIT.Balance+ Open.CREDIT.Lines + 
                   Inquiries.in.the.Last.6.Months + ll_36  + State_TX + 
                   Home.Ownership_MORTGAGE + fico,
                 data=ld_train)

fit.final=step(fit.final)

summary(fit.final)

## predicting your Original Test dataset ( Unseen data )

test.pred=predict(fit.final,newdata=ld_test)

ld_test$predicted_IR=test.pred

write.csv(test.pred,"submision1.csv",row.names = F)
