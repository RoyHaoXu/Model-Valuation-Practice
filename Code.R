rm(list = ls())

Data=read.csv(file.choose(),header=T)
names(Data)
attach(Data)
summary(Data)
str(Data)
contrasts(Region)

# Run model and find BIC & Adjusted R2
# M2 is better
M1 = lm(Units~Hours+Lines+Workers+Region,data = Data)
summary(M1)
AIC(M1)
M2 = lm(Units~Hours+Lines+Workers*Region, data = Data)
summary(M2)
AIC(M2)

# Hold-out MSE
# M4 is better
set.seed(1)
train = sample(nrow(Data),nrow(Data)*0.7)
Data.train = Data[train,]
Data.test = Data[-train,]
Units.test = Units[-train]
M3 = lm(Units~Hours+Lines+Workers+Region,data = Data.train)
summary(M3)
M4 = lm(Units~Hours+Lines+Workers*Region,data = Data.train)
summary(M4)
Units.predictM3 = predict(M3,Data.test)
Units.predictM4 = predict(M4,Data.test)
M3MSE = mean((Units.test - Units.predictM3)^2)
M4MSE = mean((Units.test - Units.predictM4)^2)
M3MSE
M4MSE

# 10 fold cross-validation
# M1 is better
set.seed(1)
k=10
M1CVMSE = rep(0,k)
M2CVMSE = rep(0,k)
folds = sample(1:k,nrow(Data),replace = TRUE)
for (i in 1:k)
{
  M1CV = lm(Units~Hours+Lines+Workers+Region,data = Data[folds!=i,])
  M1CVMSE[i] = mean((Units-predict(M1CV,Data))[folds==i]^2) 
}
for (i in 1:k)
{
  M2CV = lm(Units~Hours+Lines+Workers*Region,data = Data[folds!=i,])
  M2CVMSE[i] = mean((Units-predict(M2CV,Data))[folds==i]^2) 
}

MeanM1MSE = mean(M1CVMSE)
MeanM2MSE = mean(M2CVMSE)
MeanM1MSE
MeanM2MSE

# Select the first one because it has a smaller MSE in CV which made it better for prediction
# No outliers but could be a potential polynomial problem
M1 = lm(Units~Hours+Lines+Workers*Region, data = Data)
summary(M1)
coef(M1)
confint(M1)
plot(predict(M1,Data),residuals(M1))
plot(predict(M1,Data),rstandard(M1))

