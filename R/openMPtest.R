library(data.table)
library(caret)

dt1 <- data.table(replicate(5,sample(0:100,50,rep=TRUE)))
dt2 <- data.table(replicate(5,sample(0:100,50,rep=TRUE)))
dtList <- c("dt1", "dt2")
dtList <- mget(dtList)
mergedDT <- Reduce(function(...) merge(..., all = TRUE), dtList)