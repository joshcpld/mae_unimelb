rm(list = ls())

install.packages("readxl")
library(readxl)

date = read_excel("640101.xlsx", sheet = "Data1", col_names = "date", range = "A11:A308")
cpi = read_excel("640101.xlsx", sheet = "Data1", col_names = "cpi", range = "J11:J308")

date = date$date
cpi = cpi$cpi

plot(date,cpi,
     main = "Australia Quarterly Consumer Price Index",
     xlab = "Date",
     ylab = "Index Value",
     type = "l",
     lwd = 3,
     col = "darkorange")

n = length(cpi)

inf = 100*(cpi[2:n] - cpi[1:n-1])/cpi[1:n-1]

plot(date[2:n],inf,
     main = "Australia Quarterly Inflation",
     xlab = "Date",
     ylab = "Percentage",
     type = "l",
     lwd = 2,
     col = "darkorchid")
