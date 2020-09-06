mydata<-wine[,-c(1,2)]
sum(is.na(mydata$STARS))
#3359 out of 12795 entries are NA, decided to not use this variable. 26.5%
mydata<-mydata[,-c(2)]
colSums(is.na(mydata))
#replace missing values in residual sugar, chloride, FreeSulfurDioxide	TotalSulfurDioxide	
#Sulphates with median values.

record=c()
for(i in c(1:12795)){
  if (sum(is.na(mydata[i,]))>1){
    record<-append(record,i)
  }
}
record
length(record)
#remove 660 rows with over 1 NA
mydata<-mydata[-record,]
#now with 12135 rows, no NA in Cases (y)
colSums(is.na(mydata))
#convert things to vector and to numeric
summary(mydata)

#replace rest of NA values with median
for(i in 1:ncol(mydata)){
  mydata[is.na(mydata[i]), i] <- median(mydata[[i]], na.rm = TRUE)
}
library(lessR)
Histogram(data=mydata)
#hist of cases suggest ZIP model. 
#no categorical that i observed
#now splite into training vs testing 0.7:0.3

set.seed(123)
mydata$u <- runif(n=dim(mydata)[1],min=0,max=1);
# Create train/test split;
train.df <- subset(mydata, u<0.70);
test.df  <- subset(mydata, u>=0.70);

#first model automatic selected stepAIC OLS
#upper model
upper.lm <- lm(Cases ~ .-u,data=train.df);
summary(upper.lm)

# Define the lower model as the Intercept model

lower.lm <- lm(Cases ~ 1,data=train.df);

# Need a SLR to initialize stepwise selection
chloride.lm <- lm(Cases ~ Chlorides,data=train.df);
summary(chloride.lm)

library(MASS)

# Call stepAIC() for variable selection
stepwise.lm <- stepAIC(object=chloride.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)
AIC(stepwise.lm)
BIC(stepwise.lm)
library(car)
sort(vif(stepwise.lm),decreasing=TRUE)

plot(y=stepwise.lm$residuals,x=stepwise.lm$fitted.values)
#mean abs error=1.342, test mean abs err=1.34
mean(abs(stepwise.lm$residuals))
steptest<-predict(stepwise.lm,newdata = test.df)
step_test_residual<-steptest-test.df$Cases
mean(abs(step_test_residual))

hist(x=stepwise.lm$fitted.values)
hist(x=round(stepwise.lm$fitted.values))
hist(x=train.df$Cases)


#Second model glm-poisson
#Cases variable mean=3.028, variance=3.693, basically equal
library(lessR)
summary(mydata$Cases)
sd(mydata$Cases)^2
#model2
summary(model_poisson <- glm(Cases~.-u,data=train.df, family="poisson"))
sort(vif(model_poisson),decreasing=TRUE)
mean(abs(model_poisson$residuals))
ptest<-predict(model_poisson,newdata = test.df)
p_test_residual<-m2steptest-test.df$Cases
mean(abs(m2step_test_residual))
#sign of overfitting
plot(y=model_poisson$residuals,x=model_poisson$fitted.values)

hist(x=model_poisson$fitted.values)
hist(x=round(x = model_poisson$fitted.values))


#thirdmodel zip
library(pscl)

summary(zip1 <- zeroinfl(Cases~.-u|.-u,dist ="poisson", data = mydata)) 

summary(zip2 <- zeroinfl(Cases~VolatileAcidity+Alcohol+LabelAppeal+AcidIndex
                                   |.-(u+FixedAcidity+ResidualSugar+Density+Alcohol),dist ="poisson", data = mydata)) 

AIC(zip2)
BIC(zip2)
hist(x=zip2$fitted.values)
hist(x=round(zip2$fitted.values))

mean(abs(zip2$residuals))
ziptest<-predict(zip2,newdata = test.df)
zip_test_residual<-ziptest-test.df$Cases
mean(abs(zip_test_residual))

plot(y=zip2$residuals,x=zip2$fitted.values)

ziptest_round<-round(ziptest)
zip_round_residual<-ziptest_round-test.df$Cases
mean(abs(zip_round_residual))

ptest_round<-round(predict(model_poisson,newdata = test.df))
p_round_residual<-ptest_round-test.df$Cases
mean(abs(p_round_residual))

step_round_res<-round(steptest)-test.df$Cases
mean(abs(step_round_res))

