library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")

#Data Wrangling

head(Modal.IPI)
IPI<-na.omit(Modal.IPI)
head(IPI)
IPI<- subset (IPI, select = -Date)
head(IPI)

#Applying Linear Model

FitAll = lm( Inventory.performance.index~ ., data = IPI)
summary(FitAll)

#Testing Multicollinearity
vif(FitAll)

#Try Backward
step(FitAll, direction = 'backward')
#Test Backward
fitsome = lm(Inventory.performance.index ~ Excess.inventory.percentage + FBA.sell.through, data = MI)
summary(fitsome)

#Try Forward

IPIstart = lm(Inventory.performance.index ~ 1, data = IPI)
summary(IPIstart)
step(IPIstart, direction = 'forward', scope = (~ Excess.inventory.percentage + FBA.sell.through + Stranded.inventory.percentage + FBA.in.stock.rate))


