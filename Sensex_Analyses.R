#Loading Packages

library(dplyr)
#library (pastecs)
library (psych)
#library(lubridate)
#library (zoo)
library(readxl)
library(readr)
library(ggfortify)
library(plotly)
library(plyr)
library(rlist)
library(vars)
library(sandwich)
#Reading Files
#df <- read.csv("Sensex_Data_Analyse_New.csv")
#make sure headings are in the middle before start Processing
df_bse <-read.csv("SENSEX_FROM_BSE.csv")


#Removing Extra Column if any
#df <-within(df, rm(X,X.1,X.2,X.3,X.4 ))
#Deleting null values in the data frame
#df[ df == "null" ] <- NA
#print("Number of Missing Values :" )
#sum (is.na(df_bse)/6)
#df <- df [complete.cases(df), ]
nrow(df_bse)
#df$Date <- format(as.POSIXct(df$Date,format="%d-%m-%Y"),"%Y-%m-%d")
df_bse$Date <- format(as.POSIXct(df_bse$Date,format="%d-%b-%y"),"%Y-%m-%d")
#df$Date <- as.Date.character(df$Date)
df_bse$Date <- as.Date.character(df_bse$Date)
#df <- filter(function(x)!all(is.na(x)), df)
#Subsetting Closing Price for Return Calculation
returnTemp <- as.data.frame(df_bse$Close)
n <- nrow(returnTemp)
#Calculating Return
return <- log10(returnTemp[2:n, 1]/returnTemp[1:(n-1), 1])
return <- as.data.frame(return)
#Merging the Return with the Original Data
newRow <- data.frame(return=0)
return <- rbind(return, newRow)
df_bse <-cbind (df_bse,return)
rm (returnTemp)
rm (newRow)
rm (return)
rm  (n)
#Formatting the Decimal Places for Return
df_bse$return <- as.numeric(formatC(df_bse$return, digits = 9, format = "f"))
df_bse$return <- as.numeric(df_bse$return)

#Descriptive Stats for Return
desStats <-as.data.frame(describe (df_bse$return))
desStats <-desStats [, c("mean","sd","min","max", "kurtosis", "skew","range","n")]
m1 <- t(desStats)
desStats<- data.frame(Stats= row.names(m1), m1, row.names=NULL) 
rm (m1)
colnames (desStats)[colnames(desStats) == 'X1'] <- 'Values'
desStats$Values <-formatC(desStats$Values, digits = 9, format = "f")
acdesStats$Values<- as.numeric(desStats$Values)

#Plotting return and checking the distribution of Return

testHist <- hist(df_bse$return, 
     main="Sensex Return for 5  years", 
     xlab="Return", 
     border="black", 
     col="grey",
     breaks=15,
     prob =TRUE) 

ggplot(df_bse, aes(return))  +geom_density()
#ggplot (total, Date)

#testHist <- as.data.frame(testHist)
lines(density(df_bse$return))
d<- density(df_bse$return)
plot(d, main="Return Density")
polygon(d, col="gray", border="black")

#Preprocessing Sentiment Data
sentimentData <- read_excel("SentimentData.xlsx")
#sentimentData <- SentimentData
sentimentCopy <- sentimentData
subsettingDay <- do.call(rbind, strsplit(sentimentCopy$Title, ":"))
colnames(subsettingDay) <- c("Day", "Date")
subsettingDay <- as.data.frame(subsettingDay)
sentimentCopy <-cbind (sentimentCopy,subsettingDay)

#TransformedData <- sentimentCopy %>% 
      #  filter(!grepl('S', Day))
TransformedData <- sentimentCopy

#Formatting Factor into Date
TransformedData$Date <-format(as.POSIXct(TransformedData$Date,format="%d/%m/%Y"),"%Y-%m-%d")
TransformedData$Date <-as.Date.character(TransformedData$Date)
TransformedData <-TransformedData[order(TransformedData$Date),]
nrow(TransformedData)
nrow(df_bse)
#write.csv(df, "return.csv")
#as.Date.character(testVar$Date)
#testVar <-testVar[order(testVar$newCol),]


format(as.POSIXct(df$Date,format="%m/%d/%y"),"%d-%m-%Y")

MainData <- TransformedData[(TransformedData$Date %in% df_bse$Date),]
ReturnData <- df_bse [(TransformedData$Date %in% df_bse$Date),]

MissingDatesNew <- df_bse[!(df_bse$Date %in% MissingDates$Date),]
#Preprocessing Holiday List

Holiday_List <- read_excel("Holiday_List.xlsx")
Holiday_List$Date <- as.Date.character(Holiday_List$Date)
TransformedData <-TransformedData[!(TransformedData$Date %in% Holiday_List$Date),]
nrow(TransformedData)
diff(log10(df_bse$Close))
df_bse$Absolute_Return <- abs(df_bse$return)
#acf (df_bse$return , plot= FALSE)
#acf (df_bse$return , lag.max = 5,plot= FALSE)
df_bse$SquaredReturn <- df_bse$return^2
#AutoCorrelation of the Returns
Autocorrelation <- fortify(stats::acf(df_bse$return))
# Calculating Category frequency for the AutoCorrelation
CategoryFrequency <- list ()
CategoryFrequency <- list.append (CategoryFrequency, "1"=sum (Autocorrelation$ACF < -.1))
CategoryFrequency<-list.append(CategoryFrequency,"2"=sum (between(Autocorrelation$ACF, -.01, -.05)))
CategoryFrequency<-list.append(CategoryFrequency,"3"=sum (between(Autocorrelation$ACF, -.05, 0)))
CategoryFrequency<-list.append(CategoryFrequency,"4"=sum (between(Autocorrelation$ACF, 0, .05)))
CategoryFrequency<-list.append(CategoryFrequency,"5"=sum (between(Autocorrelation$ACF, .05, .1)))
CategoryFrequency<-list.append(CategoryFrequency,"6"=sum (.1 < Autocorrelation$ACF ))
CategoryFrequency <-data.frame(Category=names(CategoryFrequency), Frequency=unlist(CategoryFrequency))


#Plotting of Returns of t and t+1 days

returnPlot$return <- as.data.frame (df_bse$return)
n <- nrow(returnPlot)
newRow <- data.frame(return=0)
#Return_ty <- returnPlot [2:n,1]
#Return_tx <- returnPlot[1:(n-1), 1]

returntest <- data.frame(matrix(ncol = 2, nrow=n-1))
x <- c("return_tday", "returnt-1day")
colnames(returntest) <- x
returntest <- cbind(returnPlot [2:n,1], returnPlot[1:(n-1), 1])
returntest <- as.data.frame(returntest)

ap <- plot_ly(returnPlot, x = ~Return_tx, y = ~Return_ty, 
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

p<- plot_ly(total, x = ~Date, y = ~return, 
            type = 'scatter', mode = 'line', name = 'ReturnVsTime'
) 
rm( list = ls()); gc()


descriptivelist <- list ()
#descriptivelist <- list.append (a)