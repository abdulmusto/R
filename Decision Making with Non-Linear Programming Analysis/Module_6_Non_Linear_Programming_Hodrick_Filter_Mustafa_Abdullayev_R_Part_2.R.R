#### Necessary Librarie ####
library(readxl)
library(mFilter)


#### Reading Excel Data ####
my_data <- read_excel("Data.xlsx")


#### Converting it to quarterly time series data
ts <- ts(my_data$Close, frequency = 4)

#### Plotting it to observe ####
plot(ts, main = "Honeywell Quarterly Stock Data")


#### creating function for filtering with given lambda ####
my_filter <- function(lambda){
  
  data <- hpfilter(ts,freq = lambda, type = "lambda")
  plot(data)
  return(data)
  
}

my_filter(1600)



