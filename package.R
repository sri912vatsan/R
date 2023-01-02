library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)

getSymbols("FB",src="yahoo",from="2008-08-01",to="2018-08-17")

FB_log_returns<-FB%>%Ad()%>%dailyReturn(type = 'log')

head(FB_log_returns)

#FB%>%Ad()%>%chartSeries()
#FB%>%chartSeries(TA='addBBands();addVo();addMACD()',subset = 2018)

#FB%>%chartSeries(TA='addBBands()',subset = 2018)

#FB%>%chartSeries(TA='addVo()',subset = 2018)

#FB%>%chartSeries(TA='addMACD()', subset = 2018)

#getSymbols("GOOGL",src = "yahoo",from="2008-08-01",to="2018-08-17")
#getSymbols("AMZN",src="yahoo",from="2008-08-01",to="2018-08-17")
#data<-cbind(diff(log(Cl(FB))),diff(log(Cl(GOOGL))),diff(log(Cl(AMZN))))

#chart.Correlation(data)
#chart.Histogram(data)

mu<-mean(FB_log_returns)
sig<-sd(FB_log_returns)

print(sig)

N<-500

montecarlo_matrix<-matrix(nrow = 252*4,ncol = N)

montecarlo_matrix[1,1]<-as.numeric(FB$FB.Adjusted[length(FB$FB.Adjusted),])

for(j in 1:ncol(montecarlo_matrix)){
  montecarlo_matrix[1,j]<-
    as.numeric(FB$FB.Adjusted[length(FB$FB.Adjusted),])
  for(i in 2:nrow(montecarlo_matrix)){
    montecarlo_matrix[i,j]<-montecarlo_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim",seq(1,500))
name<-c('Day',name)

final<-cbind(1:(252*4),montecarlo_matrix)
final<-as.tibble(final)
colnames(final)<-name

dim(final)

final%>%gather("Simulation","Price",2:501)%>%ggplot(aes(x=Day,y=Price,Group = Simulation))+
  geom_line(alpha=0.4)+labs(title= "Facebook(FB): 500 Monte Carlo Simulations for 4 years")+theme_bw()

final[500,-1]%>%as.numeric()%>%quantile()

print("The Price prediction for Facebook")
high<-final[500,-1]%>%as.numeric()%>%quantile(1.00)
low<-final[500,-1]%>%as.numeric()%>%quantile(0.00)

print("The price can go as high as")
print(high)
print("The price can go as low as")
print(low)

                                                     
                                                    
