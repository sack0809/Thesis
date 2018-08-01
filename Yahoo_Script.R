#Loading Packages

library(dplyr)
library (psych)
library(readxl)
library(readr)
library(ggfortify)
library(plotly) 
#library(plyr)
library(rlist)
library (stargazer)
#Reading Files
rm (list = ls())
calculateReturn <- function(data){
        data <- filter(data, Close  != "null")
        data$Date <- format(as.POSIXct(data$Date,format="%Y-%m-%d"),"%Y-%m-%d")
        data$Date <- as.Date.character(data$Date)
        data$Close <- as.numeric(as.character(data$Close))
        return <- diff(log10(data$Close))
        return <- as.data.frame(return)
        #Merging the Return with the Original Data
        newRow <- data.frame(return=0)
        return <- rbind(return, newRow)
        data <-cbind (data,return)
        data$return <- as.numeric(formatC(data$return, digits = 5, format = "f"))
        
        
        return(data)
} 
showStat <- function(data){
        #calculateReturn(data)
        desStats <- describe (data$return)
        desStats <-desStats [, c("mean","sd","min","max", "kurtosis", "skew","range","n")]
        row.names(desStats) <- "Values"
        desStats <- data.frame(Stats= row.names(t(desStats)), t(desStats), row.names=NULL)
        templist <- list ()
        templist <- list.append(templist,"Zmin"={min(data$return)-mean(data$return)}/sd(data$return))
        templist <- list.append(templist,"Zmax"={max(data$return)-mean(data$return)}/sd(data$return))
        templist <-data.frame(Stats=names(templist), Values=unlist(templist))
        rownames(templist) <- NULL
        desStats <- rbind(desStats, templist)
        desStats$Values <-formatC(desStats$Values, digits = 5, format = "f")
        stargazer (desStats, summary = FALSE, rownames = FALSE, type = "text", 
                   out="table.txt", title = "Descriptive Statistics")
        
        return (desStats)
}
stylizedFacts <- function (data){
        fact1 <<- ggplot(a, aes(return))  +geom_density() +geom_vline(aes(xintercept=mean(return)),
                                                                      color="blue", linetype="dashed", size=1)
        }

a <- read.csv("Sensex_Data_Analyse_New.csv")
#sentimentData <- read_excel("SentimentData.xlsx")
#holidayList <- read_excel("Holiday_List.xlsx")

a <-calculateReturn (a)
desStats <-showStat (a)
stylizedFacts(a)




#make sure headings are in the middle before start Processing
#df <-read.csv("SENSEX_FROM_BSE.csv")


#Removing Extra Column if any
#df <-within(df, rm(X,X.1,X.2,X.3,X.4 ))
#Deleting null values in the data frame
df[ df == "null" ] <- NA
print("Number of Missing Values :" )
sum (is.na(df)/6)
df <- df [complete.cases(df), ]
nrow(df)
df$Date <- format(as.POSIXct(df$Date,format="%Y-%m-%d"),"%Y-%m-%d")
#df$Date <- format(as.POSIXct(df$Date,format="%d-%b-%y"),"%Y-%m-%d")
#df$Date <- as.Date.character(df$Date)
df$Close <- as.numeric(as.character(df$Close))
#df <- filter(function(x)!all(is.na(x)), df)
#Subsetting Closing Price for Return Calculation
returnTemp <- as.data.frame(diff(log10(df$Close)))
#data.frame(Close=names(returnTemp)
#df$return1 <-diff(log10(df$Close))
n <- nrow(returnTemp)
#Calculating Return
#return <- log10(returnTemp[2:n, 1])/(returnTemp[1:(n-1), 1])
return <- diff(log10(df$Close))
return <- as.data.frame(return)
#Merging the Return with the Original Data
newRow <- data.frame(return=0)
return <- rbind(return, newRow)
df <-cbind (df,return)
rm (returnTemp)
rm (newRow)
rm (return)
rm  (n)
#Formatting the Decimal Places for Return
df$return <- as.numeric(formatC(df$return, digits = 9, format = "f"))
df$return <- as.numeric(df$return)

#Descriptive Stats for Return
desStats <-as.data.frame(describe (a$return))
m1 <- t(desStats)
desStats<- data.frame(Stats= row.names(m1), m1, row.names=NULL) 
rm (m1)
colnames (desStats)[colnames(desStats) == 'X1'] <- 'Values'
desStats$Values <-formatC(desStats$Values, digits = 5, format = "f")
desStats$Values<- as.numeric(desStats$Values)
#stargazer(desStats, type = "text", title="Descriptive statistics", digits=1, out="table1.txt", flip = TRUE)
capture.output(desStats, file = "basicsStats.txt")
cat("Statistical Values", file = "basicsStats.txt", append = TRUE)
#Plotting return and checking the distribution of Return

testHist <- hist(a$return, 
                 main="Sensex Return for 5  years", 
                 xlab="Return", 
                 border="black", 
                 col="grey",
                 breaks=15,
                 prob =TRUE) 

ggplot(a, aes(return))  +geom_density() +geom_vline(aes(xintercept=mean(return)),
                                                   color="blue", linetype="dashed", size=1)

#testHist <- as.data.frame(testHist)
lines(density(a$return))
d<- density(a$return)
plot(d, main="Return Density")
polygon(d, col="gray", border="black")

#Preprocessing Sentiment Data

#sentimentData <- SentimentData
sentimentCopy <- sentimentData
subsettingDay <- do.call(rbind, strsplit(sentimentCopy$Title, ":"))
colnames(subsettingDay) <- c("Day", "Date")
subsettingDay <- as.data.frame(subsettingDay)
sentimentCopy <-cbind (sentimentCopy,subsettingDay)

TransformedData <- sentimentCopy %>% 
  filter(!grepl('S', Day))
#TransformedData <- sentimentCopy

#Formatting Factor into Date
TransformedData$Date <-format(as.POSIXct(TransformedData$Date,format="%d/%m/%Y"),"%Y-%m-%d")
TransformedData$Date <-as.Date.character(TransformedData$Date)
TransformedData <-TransformedData[order(TransformedData$Date),]
nrow(TransformedData)
nrow(df)
#write.csv(df, "return.csv")
#as.Date.character(testVar$Date)
#testVar <-testVar[order(testVar$newCol),]


format(as.POSIXct(df$Date,format="%m/%d/%y"),"%d-%m-%Y")

MainData <- TransformedData[(TransformedData$Date %in% df$Date),]
MissingData <- df [!(TransformedData$Date %in% df$Date),]

MissingDatesNew <- df[!(df$Date %in% MissingDates$Date),]
#Preprocessing Holiday List

Holiday_List$Date <- as.Date.character(Holiday_List$Date)
TransformedData <-TransformedData[!(TransformedData$Date %in% Holiday_List$Date),]
nrow(TransformedData)

df$Absolute_Return <- abs(df$return)
#acf (df$return , plot= FALSE)
#acf (df$return , lag.max = 5,plot= FALSE)
df$SquaredReturn <- df$return^2
#AutoCorrelation of the Returns
Autocorrelation <- fortify(stats::acf(a$return))
# Calculating Category frequency for the AutoCorrelation
CategoryFrequency <- list ()
testlist <- list ()
CategoryFrequency <- list.append (CategoryFrequency, "1"=sum (Autocorrelation$ACF < -.1))
CategoryFrequency<-list.append(CategoryFrequency,"2"=sum (between(Autocorrelation$ACF, -.01, -.05)))
CategoryFrequency<-list.append(CategoryFrequency,"3"=sum (between(Autocorrelation$ACF, -.05, 0)))
CategoryFrequency<-list.append(CategoryFrequency,"4"=sum (between(Autocorrelation$ACF, 0, .05)))
CategoryFrequency<-list.append(CategoryFrequency,"5"=sum (between(Autocorrelation$ACF, .05, .1)))
CategoryFrequency<-list.append(CategoryFrequency,"6"=sum (.1 < Autocorrelation$ACF ))
CategoryFrequency <-data.frame(Category=names(CategoryFrequency), Frequency=unlist(CategoryFrequency))

if (Autocorrelation$ACF < -.1)
{
   testlist <- list.append(testlist)     
}

#Plotting of Returns of t and t+1 days

returnPlot <- as.data.frame (a$return)
n <- nrow(returnPlot)
Return_ty <- returnPlot [2:n,1]
Return_tx <- returnPlot[1:(n-1), 1]

p<- plot_ly(total, x = ~, y = ~Return_ty, 
             type = 'scatter', mode = 'markers', name = 'Return 1'
) 
g <- ggplot(data.frame (Return_tx =returnPlot[1:(n-1), 1], 
                        Return_ty =returnPlot [2:n,1]), aes(Return_tx, Return_ty)) + 
        geom_point( size=2)  + theme(
                panel.background = element_rect(fill = NA),
                panel.grid.major  =   element_line(colour = "grey50"),
                panel.ontop = FALSE
        )

observedValue <- list ()


whatpercent <- function(x, k) {
        mean(abs(x - mean(x)) <= sd(x) * k)
}

Box.test (x, lag = 1, type = "Ljung")

total <- merge(df, TransformedData ,by="Date")
write.csv("total.csv")

return <- as.data.frame(diff(log10(df$Close)))
return <- as.data.frame(return)
