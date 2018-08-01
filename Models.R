library(vars)
library(sandwich)
library(forecast)
library(dynlm)
library(stargazer)
library(broom)
library(apsrtable)
library (xtable)
ar.test <-auto.arima(total$return)
totalcopy <- total
returntest <- ts (returntest, start = 1,end= 1469)
NegativeSentiment$negativeSentiment <- ts (total$Negativ, start = 1,end= 1469)
ReturnARmodel <- dynlm(`return` ~ L( return,1)+L( return,2)+L( return,3)+L( return,4)+L( return,5), data=totalcopy)
Martingalmodel<- dynlm(`return` ~ L(return,1) , data=totalcopy)
SentARmodel <- dynlm (`Negativ` ~ L (`Negativ`, 1:5), data=totalcopy)

totalcopy$return <- ts (totalcopy$return, start =1 , end = 1467)
totalcopy$Negativ <- ts (totalcopy$Negativ, start =1 , end = 1467)
Varmodel <- dynlm (`return` ~ L (`Negativ`, 1:5)+L( return,1)+L( return,2)+L( return,3)+L( return,4)+L( return,5), data=totalcopy)

VarNeweywest <- testNewest <- coeftest(Varmodel, vcov. = NeweyWest)



stargazer::stargazer( Martingalmodel, ReturnARmodel,SentARmodel, Varmodel, VarNeweywest, object.names = TRUE,
                      
                      type="text", title="Panel Regression", align = TRUE,
                       
                      covariate.labels=c("Return t-1","Return t-2","Return t-3", "Return t-4","Return t-5",
                                        "Negative t-1", "Negative t-2", "Negative t-3", "Negative t-4", "Negative t-5"),
                     omit.stat=c("LL","ser"),model.names = FALSE, dep.var.labels.include = FALSE,model.numbers = FALSE,
                   out="models.txt")

# model1 <- stargazer::stargazer(returnARmodel, negsentARmodel, type="text", title="Panel Regression", align = TRUE,
#                      
#                      covariate.labels=c("Return t-1","Return t-2","Return t-3", "Return t-4","Return t-5",
#                                         "Negative1", "Negative2", "Negative3", "Negative4", "Negative5"),
#                      omit.stat=c("LL","ser"),model.names = FALSE, dep.var.labels.include=TRUE, model.numbers = TRUE,
#                      multicolumn = TRUE,out="models.txt")
# 
# #model2 <- stargazer::stargazer( var.aic$varresult$retur, type="text", title="Panel Regression", align = TRUE,
#                      dep.var.labels=c("Return+Sentiment"), 
#                      covariate.labels=c("Return t-1","Return t-2","Return t-3", "Return t-4","Return t-5",
#                                         "Negative1", "Negative2", "Negative3", "Negative4", "Negative5",
#                                         "Constant"),
#                      omit.stat=c("LL","ser","f"),model.names = FALSE, dep.var.labels.include=TRUE, out="modelsVAR.txt")


# dep.var.labels = c("returnMar", "returnARmodel","negsentARmodel", "vardlm", "testNewest"), multicolumn = TRUE,
# column.labels = c("returnMar", "returnARmodel","negsentARmodel", "vardlm", "testNewest"),
# column.separate = c(1,2,3,4,5),