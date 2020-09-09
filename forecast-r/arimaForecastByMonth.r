
library(forecast)
require(smooth)
library(TSstudio)

file = "ForecastData.csv"
data = read.csv(file, sep=";", header=FALSE)

# Index starts at 1
may2019Index = 29
jun2019Index = 30  
jul2019Index = 31

runModels = function(mappedData, name) {
  
  realData = ts(mappedData, start = c(2018,1),end=c(2020, 7 ), frequency = 12)
  data1 = ts_ma( ts(mappedData, start = c(2018,1),end=c(2020, 3), frequency = 12), plot=FALSE)$ma_6
  #data1 = ts(mappedData, start = c(2018,1),end=c(2020, 4), frequency = 12)
  #plot(realData)

  
  model = ets(data1)
  
  values = forecast(model, 3)
  #print("ets")
  #print(values$mean)
  
  #plot(forecast(model, 3))
  
  if ( realData[may2019Index] ==0 | realData[jun2019Index] == 0  | realData[jul2019Index] == 0 ) {
    resultEts = 10001
  }
  else {
    result = (abs(realData[may2019Index] - values$mean[1])/ realData[may2019Index]) + (abs(realData[jun2019Index] - values$mean[2])/ realData[jun2019Index]) + (abs(realData[jul2019Index] - values$mean[3])/ realData[jul2019Index])
    resultEts = 100 * (result/3)
  }
  
  #print(resultEts)
  
  model = auto.arima(data1)

  values = forecast(model, 3)
  
  #print(values$mean)
  
  #plot(forecast(model, 3))
  
  if ( realData[may2019Index] ==0 | realData[jun2019Index] == 0  | realData[jul2019Index] == 0 ){
    resultArima = 10000
  }
  else {
    result = (abs(realData[may2019Index] - values$mean[1])/ realData[may2019Index]) + (abs(realData[jun2019Index] - values$mean[2])/ realData[jun2019Index]) + (abs(realData[jul2019Index] - values$mean[3])/ realData[jul2019Index])
    resultArima = 100 * (result/3)
  }
  
  
  if (resultEts < resultArima){
    
    model = ets(realData)
    newValues = forecast(model, 3)
    result = resultEts
    modelName = "ets"
  } else {
    model = auto.arima(realData)
    values = forecast(model, 3)
    result = resultArima
    modelName = "arima"
  }
  

  #write.table(paste(name, modelName, realAugust, forecastAugust, sep= "," ), "./augustForecast.csv", append = T, col.names=FALSE, row.names=FALSE )
  
  write.table(paste(name, modelName, result ,ceiling(values$mean[1]), ceiling(values$mean[2]), ceiling(values$mean[3]) , sep = ",") , "./forecastwithmape3.csv", append = T, col.names=FALSE, row.names=FALSE )
  
  
}


for (row in 1: nrow(data)) {
  tempData = c()
  SKU = ""
  for (col in 0: ncol(data)){
    if( col == 1){
      # Extract sku
     SKU = data[row,col] 
    }
    if (col > 1){
      tempData = c(tempData, data[row,col])
      
    }
  }
  runModels(tempData, SKU)
  
}


