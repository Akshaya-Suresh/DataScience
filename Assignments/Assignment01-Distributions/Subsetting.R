install.packages("rJava")
install.packages("XLConnect")
library(XLConnect)
wBook = loadWorkbook("ADS/Assignment/loan.xlsx") 
dframe = readWorksheet(wBook, sheet="loan")
myData <- data.frame(dframe)
filteredData <- subset(myData, Age>30  & Occupation == "unemploye")
createSheet(wBook, name = "FilteredSheet")
writeWorksheet(wBook, filteredData, sheet = "FilteredSheet", startRow = 1, startCol = 1)
saveWorkbook(wBook)


