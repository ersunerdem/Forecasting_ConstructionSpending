rm(list=ls()) #Clear working space

#Packages
library(fpp2)
library(fpp3)
library(tidyverse)
library(tsibble)
library(dplyr)

data("uschange")

library(vars)
VARselect(uschange[,1:2], lag.max=8,
          type="const")[["selection"]]

df <- uschange[,1:2]

var1 <- VAR(uschange[,1:2], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")
var2 <- VAR(uschange[,1:2], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")
var3 <- VAR(uschange[,1:2], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

forecast(var3) %>%
  autoplot(PI=FALSE) + xlab("Year")
