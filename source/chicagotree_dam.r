# This is extracted from Richard's chicago_tree.r program

prepareOneSpecies=function(full=ctree,s='Acer negundo',spcol='GENUSSPECI')
{
      full[,'occur']=0
      full[full[,spcol]==s,'occur']=1
      return(full)
}
 
graphOneResult=function(full=ctree,mod,sp='Acer saccharinum',predictor='BLDG_AGE',predictor2=NULL,which2='mean',yrange=NULL,add=FALSE,div=100,Ncat=20, retSpecs=FALSE)
{
 allpredictors=rownames(mod[[sp]]$cf)[-1]   ## Read predictors out of the model result. Eliminate first with -1 because that's intercept.
 fullx=oneFittedResult(full=full,sp=sp,pred1=predictor,pred2=predictor2,allpredictors=allpredictors,whichpred2=which2,div=div)
 #DAM - prevent conversion from data fram to vector is only one predictor
 fullx=fullx[,allpredictors, drop=FALSE]                ## Predictors must be in same order as they appear in model result
 # browser()
 
 cf=mod[[sp]]$cf[,1]
 y=logistic.standard(x=fullx,param=cf)
 
 # DAM - return 2 matricies with the x,y coordinates instead of plotting
 if (retSpecs) {
    regression_coords = matrix(c(fullx[,predictor], y[,1]), nrow=length(fullx[,predictor]), dimnames=list(NULL,c('x','y')))
    occurence_coords = graphOneResultBins(full=full,sp=sp,predictor=predictor,add=TRUE,div=Ncat, retSpecs=TRUE)
    return (list(regression_coords, occurence_coords))
 }
 else {
    if(is.null(yrange)) yrange=c(0,1)
    if(which2!='mean') lineclr='gray'
    else lineclr='black'
    if(!add) plot(fullx[,predictor],y[,1],type='l',ylim=yrange,xlab=predictor,ylab='probability of occurrence',col=lineclr)
    else lines(fullx[,predictor],y[,1],col=lineclr)
    graphOneResultBins(full=full,sp=sp,predictor=predictor,add=TRUE,div=Ncat)
  }
 # return(y)
}


#DAM - remove default for pred2
# Construct 3 dataframe of all predictors, with one predictor varying from its min to max in div=100 units. A second predictor can be held at either its mean, mean+1SD, or mean-1SD. The latter is controlled by the argument whichpred2, set to either 'mean', 'upper', 'lower'.
oneFittedResult=function(full=ctree,sp='Acer saccharinum',pred1='BLDG_AGE',pred2=NULL,whichpred2='mean',
                         allpredictors=c('BLDG_AGE','HU_DENS','DIST_WATER','HEIGHT_MAX','CROWN_AREA'),div=100)
{
 
 x=full[,pred1]
 graphx=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),len=div)	
 fullx=data.frame(graphx)
 
 colnames(fullx)=pred1
 #DAM - this fails with a factor predictor. eg LU1 is being passed in a predictor but is not a column in "full"
 
 for(onepred in allpredictors) if(onepred!=pred1) fullx[,onepred]=mean(full[,onepred],na.rm=TRUE)
 if (!is.null(pred2))
 {
    if(whichpred2=='upper') fullx[,pred2]=mean(full[,pred2],na.rm=TRUE)+2*sd(full[,pred2],na.rm=TRUE)
    else if(whichpred2=='lower') fullx[,pred2]=mean(full[,pred2],na.rm=TRUE)-2*sd(full[,pred2],na.rm=TRUE)
 }
	
 return(fullx)
}


# Using observations only, find a fitted probability by binning data into a set of equal quantiles for one predictor. The number of equal quantiles can be adjusted with the argument div.
graphOneResultBins=function(full=ctree,mod,sp='Acer saccharinum',predictor='BLDG_AGE',div=20,clr='red',add=FALSE, retSpecs=FALSE)
{
 spdata=prepareOneSpecies(full=full,s=sp)
 x=spdata[,predictor]
 
 bins=unique(quantile(x,prob=seq(0,1,len=div),na.rm=TRUE))
 Nbin=length(bins)
 bins[Nbin]=1+bins[Nbin]                  ## Necessary so the highest x falls in the last bin
 # browser()
 xcat=cut(x,breaks=bins,right=FALSE)
 
 meanoccur=tapply(spdata$occur,xcat,mean)
 meanx=tapply(x,xcat,mean)
 ord=order(meanx)
 
 # DAM - return a matrix with the x,y coordinates instead of plotting
 if (retSpecs) {
 #  return (data.frame(x=meanx[ord], y=meanoccur[ord]))
   return (matrix(c(meanx[ord], meanoccur[ord]), nrow=length(meanx[ord]), dimnames=list(NULL,c('x','y'))))
 }
 else {
   if(add) points(meanx[ord],meanoccur[ord],pch=16,col=clr)
   else plot(	meanx[ord],meanoccur[ord],pch=16,ylim=c(0,1),xlab=predictor,ylab='probability of occurrence')
 }
}


# Standard logistic function. In basic use, there are two param, intercept a and slope b. Any number of predictors allowed in dataframe x (one predictor per columns).
# Basic: y = exp(a+x%*%b)/(1+exp(a+x%*%b))  [%*% is matrix multiplication in R]
# One or two additional parameters are allowed, a basement and an asymptote. In basic, max(y)=1 and min(y)=0. 
## If param[3] exists, then it's the asymptote and max(y)=param[3].
## If param[4] exists, then it's the basement and min(y)=param[4].
logistic.standard=function(x,param,log=FALSE,...)
{
 x=as.matrix(x)
 nopredictor=dim(x)[2]
 a=param[1]
 b=param[2:(nopredictor+1)]
 
 asymp=ifelse(length(param)>nopredictor+1,param[nopredictor+2],1)
 basement=ifelse(length(param)>nopredictor+2,param[nopredictor+3],0)

 X=x%*%b
 pwr=a+X
 y=invlogit(pwr)
 prob=y*(asymp-basement)+basement

 infinite.pos=which(is.infinite(exp(pwr)))
 prob[infinite.pos]=basement+asymp

 if(log) return(log(prob))
 return(prob)
}

## Inverse of logistic.standard, 2-parameter version only (no basement nor ceiling)
invlogit=function(x) return(exp(x)/(1+exp(x)))


