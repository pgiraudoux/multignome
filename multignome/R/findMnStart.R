"findMnStart" <-
function(mnData,expData){
  # This function is an attempt to derive reasonable starting values for multinomial models based on independant binomial glms
  nulResp <- mnData[,1]    # Null responses, eg. empty traps
  mnData <- as.data.frame(mnData[,-1])
  colnames(mnData) <- paste("S", 1:ncol(mnData), sep="")
  trms <- colnames(expData) <- paste("V", 1:ncol(expData), sep="")
  trms <- paste(trms,sep="+")
  
  for (i in 2:length(trms)) trms[1] <- paste(trms[1],trms[i],sep="+")
  trms <- trms[1]
  Coefs <- matrix(NA,nrow=ncol(expData),ncol=ncol(mnData))
  expD <- as.data.frame(expData)
  attach(expD)
  
  for (i in 1:ncol(mnData)){
    y <- cbind(mnData[,i],nulResp)
    aModel <- glm(y ~ 1, family="binomial")
    aModel <- update(aModel, formula(paste(".~.-1+", trms, sep = "")))
    Coefs[,i] <- aModel$coef 
  }
  detach(expD)
  Coefs                      
}

