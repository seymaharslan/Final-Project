#Data Wrangling

head(Modal.Sales)
Sales<-na.omit(Modal.Sales)
Sales<- subset (Sales, select = -Aplus)
Sales<- subset (Sales, select = -SKU)
Sales<- subset (Sales, select = -ASIN)
head(Sales)

#Try Linear Modal
FitAllSales = lm( QTY.Sold~ ., data = Sales)
summary(FitAllSales)
LMviews= lm( Views~ ., data = Sales)
summary(LMviews)
LMviews1= lm( Views~ Price + Rating + Content, data = Sales)
summary(LMviews1)

#Testing Multicollinearity
vif(FitAllSales)

#Try Backward
step(FitAllSales, direction = 'backward')

#Try Forward
Salesstart = lm(QTY.Sold ~ 1, data = Sales)
summary(Salesstart)
step(Salesstart, direction = 'forward', scope = (formula(FitAllSales)))

#Try Hybrid
step(Salesstart, direction="both", scope=formula(FitAllSales))
