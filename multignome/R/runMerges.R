runMerges <- function(formula,data,fit){
# library(nnet)
mergeL <- list()
AICL <- list()
mergeNames <- list()

## First run of mergeLevels
data1 <- data
merge0 <- mergeLevels(formula,data1,mergeFactor="habitat")
print(min(merge0))
write.table(merge0,"merge0.txt")
newMod <- update(fit, data=data1,trace=FALSE)
AICL[[1]] <- newMod$AIC
## Set number of merges
N <- nlevels(data1$habitat)*nlevels(data1$habitat)
## Iterate the merges:
for (i in 2:N){
if (min(merge0)< 0) {
## Create a new data set which contains merge one
datanew <- data1
A <- attr(merge0,"mostLikelyMerge")
L <- levels(datanew$habitat)
L[is.element(L,A)] <- paste(A[1],A[2],sep="_")
mergeNames[[i]] <- paste(A[1],A[2],sep="_")
print(paste("merge n",i-1,mergeNames[[i]]))
levels(datanew$habitat) <- L
## merge
merge <- mergeLevels(formula,datanew,mergeFactor="habitat")
mergeL[[i]] <- merge
## get AIC for the new model: 
newMod <- update(fit, data=datanew,trace=FALSE)
AICL[[i]] <- newMod$AIC
## Set parameters for the next merge:
data1 <- datanew
merge0 <- mergeL[[i]]
}
else print(paste(i,"merges done",":no more merge posssible"))
}
## Compare old and new classifications
T <- table(data$habitat, datanew$habitat)
output <- list(length(mergeL),datanew,newMod,merge0,T,mergeL,AICL)
names(output) <- c("Nmerges","datanew","new_model","last_merge","Table","list_merges","list_AIC")
output
}



