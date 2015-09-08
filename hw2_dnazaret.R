# =====================================================================
# CSE 587
# Author: Daniel Nazareth
# Email: dnazaret@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)

# need to read the stocklist, edit faulty data and loop all files

file_list = list.files(pattern=".csv$",path="/gpfs/courses/cse587/spring2015/data/hw2/data/")
stocklist = scan(file="/gpfs/courses/cse587/spring2015/data/hw2/stocklist.txt",what=character(),sep="\n")
stocklist[match("JASOLtd.",stocklist)] <- "JASO"  #files with name inconsistency
stocklist[match("ULTAInc.",stocklist)] <- "ULTA"
stocklist = paste(stocklist,".csv",sep="")
final_filename_vector <- character()
MAE_LinReg_vector <- double()
MAE_Holt_vector <- double()
MAE_Arima_vector <- double()
setwd("/gpfs/courses/cse587/spring2015/data/hw2/data/")
for(filename in file_list)
{


# just read one file
#filename = "/gpfs/courses/cse587/spring2015/data/hw2/data/AAPL.csv"

# if file is not empty and file length >= 755 lines and file exists in stocklist
if((file.info(filename)[1]>0) && (length(readLines(filename))>=755) && (match(filename, stocklist, nomatch=-1)>0)) {
  
  # read one csv file into variable (DO NOT EDIT)
  textData=read.csv(file=filename, header=T)
  
  # convert txt data to time-series data, in day unit (DO NOT EDIT)
  tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
  
  # define train data (DO NOT EDIT)
  trainData = window(tsData, end=c(2014,14))
  
  # define test data (DO NOT EDIT)
  testData = window(tsData, start=c(2014,15))
             
  # MAE row vector (DO NOT EDIT)
  MAE = matrix(NA,1,length(testData))
  
  MAE_hw = matrix(NA,1,length(testData))
  
  MAE_lr = matrix(NA,1,length(testData))
  
  # apply ARIMA model (DO NOT EDIT)
  fitData = auto.arima(trainData,lambda=NULL,seasonal=FALSE,approximation=TRUE) #inital configuration timing out on CCR hence changed parameters
  
  fitData_LinReg = tslm(trainData ~ trend) # applying Linear regression model
  
  fitData_HoltWinter = HoltWinters(trainData,gamma=FALSE) #apply Holt Winters Model
  
  
  # apply forecast(DO NOT EDIT)
  forecastData = forecast(fitData, h=length(testData))
  
  forecastData_LinReg = forecast(fitData_LinReg, h=length(testData))
  
  forecastData_HoltWinter = forecast(fitData_HoltWinter, h=length(testData))
  
  # print variable and see what is in the result data set
  print(forecastData)
  
  # calculate Mean Absolute Error 
  for(i in 1:length(testData))
  {
    MAE[1,i] = abs(forecastData$mean[i] - testData[i])
  }
  
  for(i in 1:length(testData))
  {
    MAE_hw[1,i] = abs(forecastData_HoltWinter$mean[i] - testData[i])
  }
  
  for(i in 1:length(testData))
  {
    MAE_lr[1,i] = abs(forecastData_LinReg$mean[i] - testData[i])
  }
  
  final_filename_vector <- append(final_filename_vector, filename)
  MAE_LinReg_vector <- append(MAE_LinReg_vector,sum(MAE_lr[1,1:10]))
  MAE_Holt_vector <- append(MAE_Holt_vector,sum(MAE_hw[1,1:10]))
  MAE_Arima_vector <- append(MAE_Arima_vector,sum(MAE[1,1:10]))
  
}# end if

} # end for loop
  
frame_LinReg = data.frame(StockName_LinReg=final_filename_vector, LinReg_MAE=MAE_LinReg_vector)
frame_Holt = data.frame(StockName_Holt=final_filename_vector, Holt_MAE=MAE_Holt_vector)
frame_Arima = data.frame(StockName_Arima=final_filename_vector, Arima_MAE=MAE_Arima_vector)

sorted_frame_LinReg = frame_LinReg[order(MAE_LinReg_vector),]
sorted_frame_Holt = frame_Holt[order(MAE_Holt_vector),]
sorted_frame_Arima = frame_Arima[order(MAE_Arima_vector),]

print("--------------------Results For Linear Regression Model-------------------------")

print(sorted_frame_LinReg[1:10,])

cat("\n\n\n\n\n")

print("--------------------Results For Holt Winters Model--------------------------------")

print(sorted_frame_Holt[1:10,])

cat("\n\n\n\n\n")

print("--------------------Results For Arima Model--------------------------------")

print(sorted_frame_Arima[1:10,])

setwd("/gpfs/courses/cse587/spring2015/students/dnazaret/hw2/")
num <- c(1:10)

jpeg('lm.jpg')
plot(num,sorted_frame_LinReg[1:10,2], xlab="Linear Regression 10 Minimum  Files", ylab="MAE",xlim=c(1,10), ylim=c(0,0.5),col="blue")
lines(sorted_frame_LinReg[1:10,2], lw=4, col="red")
dev.off()

jpeg('hw.jpg')
plot(num,sorted_frame_Holt[1:10,2], xlab="Holt-Winters 10 Minimum Files", ylab="MAE",xlim=c(1,10),ylim=c(0,0.5), col="blue")
lines(sorted_frame_Holt[1:10,2], lw=4, col="red")
dev.off()

jpeg('arima.jpg')
plot(num,sorted_frame_Arima[1:10,2], xlab="Arima 10 Minimum MAE Files", ylab="MAE",xlim=c(1,10),ylim=c(0,0.5), col="blue")
lines(sorted_frame_Arima[1:10,2], lw=4, col="red")
dev.off()

  # this is the result you need for stock AAPL
  #print(sum(MAE[1,1:10]))
  
  # plot the top 10 minimum sum of MAE in 3 models respectively
  #plot(MAE[1,1:10], col = "blue")
  #lines(MAE[1,1:10], lw = 2, col = "red")
  ### TO DO


