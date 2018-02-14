## Database of Chicago trees. This file reads data, formats for any one species, and executes the logistic model with R's glm. Included is a cover function that will loop through many species and save model results for all.
## This is self-contained, but 2 lines must be adjusted for the computer it is to be run on.

## Nine functions are included
#  1) readChicagoTree     ## Reads tab-delimited version of data Trees_DuPage_to_Rick.xlsx into R dataframe
#  2) prepareOneSpecies   ## Uses the dataframe created by readChicagoTree and formats for a selected species
#  3) modelOneSpecies     ## Uses one species' data created by prepareOneSpecies and executes logistic regression on occurrence
#  4) modelAllSpecies     ## Repeats model for any number of species chosen
#  5) graphOneResult      ## Graphs results of model for one species, occurrence vs. one chosen predictor, overlaying observed occurrence from graphOneResultBins
#  6) oneFittedResult     ## Create a dataframe with one predictor varied, a second held at either its mean or 2SD above or below, the rest at their means
#  7) graphOneResultBins  ## Graphs observed occurrence probabilities in bins of one chosen predictor
#  8) fullFittedMatrix    ## Uses model results to find the fitted occurrence probability at every point in the full data for every species
#  9) logistic.standard   ## The logistic equation, needed for graphOneResult
# 10) invlogit            ## The inverse logistic function, needed for graphOneResult

## Sample session. 
# First save Trees_DuPage_to_Rick.xlsx sheet Trees in DuPage as tab-delimited text named Trees_DuPage_to_Rick.csv 
# Also save Trees_DuPage_to_Rick.xlsx sheet Common species as tab-delimited text named Trees_DuPage_to_Rick_species.csv 
## The rest is executed from R command line
# source('D:/CRTI/r_projects/chicago_tree_dam/chicagotree_dam.r')  ## THIS LINE MUST CHANGE FOR WINDOWS OR MAC!
# DAM - select only HEIGHT_MAX>0 & DBH_IN>0
# ctree=subset(readChicagoTree(),HEIGHT_MAX>0 & DBH_IN >0)
# fullmod=modelAllSpecies(full=ctree,spp=commonSpecies,predictors=c('BLDG_AGE','HU_DENS','DIST_WATER','HEIGHT_MAX','CROWN_AREA'))
# fullmod[['Acer saccharinum']]
# fullmod[['Acer saccharum']]
# graphOneResult(full=ctree,fullmod,sp='Acer saccharinum',predictor='BLDG_AGE')
#
# graphOneResult(full=ctree,mod,sp='Acer saccharinum',predictor='BLDG_AGE',
#                allpredictors=c('BLDG_AGE','HU_DENS','DIST_WATER','HEIGHT_MAX','CROWN_AREA'))


###############################################################################################################################3
## The folder where data reside, relative to R's working directory. Written differently on Windows, Macs, Unix
## Following 2 lines must be changed to reflect computer in use.
chgotreepath='D:/CRTI/data/'    
# source(paste(chgotreepath,'chicagotree.r'))      ## Sourcing this file, indicated by full path relative to working directory.

## The full excel file, 'Trees_DuPage_to_Rick.xlsx', has every record: individual trees with species names and various independent variables.
## Convert to tab-delimited text for this function. 
## It takes a few seconds to load the big table, so it's better to run this once at the start of a session to create an R object with the full table, here named ctree.
## Note extreme errors: some HEIGHT_MAX ~ -1e30
# ctree=readChicagoTree()

readChicagoTree=function(infile='cleaned/dupage_county_accepted_V1.csv',path=chgotreepath)
{
 input=paste(chgotreepath,infile,sep='')
 y=read.delim(input,as.is=TRUE,sep=',')      ## Use as.is=TRUE to make sure species names are characters, not factors
 return(y) 
}

# Same excel file has a sheet with species names. Save as tab-delimited text 'Trees_DuPage_to_Rick_species.csv'
commonSpecies=unique(read.delim(paste(chgotreepath,'Trees_DuPage_to_Rick_species.csv',sep=''),as.is=TRUE,header=FALSE))
landUse=read.delim(paste(chgotreepath,'Trees_DuPage_to_Rick_land_use.csv',sep=''),as.is=TRUE,header=FALSE)

## For modeling a single species, a table is needed for that species. A column occur is created
## occur is set = 1 for each record of that species and = 0 for all other species records. 
## This function creates such a table for one species, starting with result of readChicagoTree and passing one Latin name.
## The argument spcol allows the name of the column carrying the species to be changed. It defaults to the original.
prepareOneSpecies=function(full=ctree,s='Acer negundo',spcol='GENUSSPECI')
{
 	full[,'occur']=0
 	full[full[,spcol]==s,'occur']=1
 	return(full)
}

## First create data for a single species, eg
# tadist=prepareOneSpecies(full=ctree,s='Taxodium distichum')
## This runs glm for a single species (or any data submitted) with predictors submitted as an argument so they can be varied
## An R formula is written occur ~ x1+x2+... for as many predictors x as desired. The variables x1 etc must be columns in the data.frame, as is occur.
## So a model with just two predictors is written glm(occur~BLDG_AGE+HU+DENS,data=spdata)
## This function is set up to create a formula for any predictors submitted. This requires the R function as.formula. 
## It returns a list with two parts of the model, 1) a table of coefficients and 2) the aic
modelOneSpecies=function(spdata=tadist,predictors=c('BLDG_AGE','HU_DENS','DIST_WATER','HEIGHT_MAX','CROWN_AREA'))
{
	formulax=paste(predictors,collapse='+')              ## This builds a character string of all predictors separated by +
	fullformula=as.formula(paste('occur ~ ', formulax))  ## This creates the R style formula 
	
#	model=glm(formula=fullformula,family=binomial(link='logit'),data=spdata)
	model=glm(formula=fullformula,family=binomial(link='logit'),data=spdata, control=list(maxit=50))
	
	return(list(cf=summary(model)$coefficients,aic=summary(model)$aic))
}


#DAM -rewrote to properly create a mode for each land use and return them in the list
# Repeat the model for each of the commonSpecies and return a list of results for each
modelAllSpecies=function(full=ctree,spp=commonSpecies,lu=landUse,predictors=c('BLDG_AGE','HU_DENS','DIST_WATER','HEIGHT_MAX','CROWN_AREA'))
{
      # DAM This gets rid of the warning message
#    spp = spp[1][spp[1] != "Malus tschonoskii"] 
    
      models <- list()
      # Create a model for this species irrespective of land use. List index is [[<species name.]]
      for(onespp in spp[,1]) 
      {
            models[[onespp]] <- modelOneSpecies(spdata=prepareOneSpecies(full=full,s=onespp),predictors=predictors) 
#            cat("\nFinished model for species",onespp,"\n")
            
            # Create a model for each species/land use combination. List index is [[<species name]][[<land use desc>]]
            for(onelu in c(2,3,4,5,6,7,8,9,11))
            {
                  oneludesc <- landUse$V2[match(onelu+1,landUse$V1)]
                  models[[onespp]][[oneludesc]] <- modelOneSpecies(spdata=prepareOneSpecies(full=subset(full,LU == onelu),s=onespp), predictors=predictors) 
#                  cat("Finished model for species", onespp, paste(oneludesc),"\n")
            }     
      }
      return(models) 
}


#TEMP
top_spp = function (data_frame, spp_column, cutoff)
{
  #Get just the top species names
  top_spp <-
    names(head(sort(table(
      factor(data_frame[[spp_column]])
    ), decreasing = TRUE), ifelse(cutoff==0, .Machine$integer.max, cutoff)))
  top_tree_data <- data_frame
  top_tree_data[[spp_column]] = ifelse ((match(top_tree_data[[spp_column]], top_spp, nomatch = 0) > 0), top_tree_data[[spp_column]], "Other")
  return(top_tree_data)
}

library(ggplot2)



#OBSOLETE
graphOnePredictor_works=function(full=ctree,mod,sps='Acer saccharinum',predictor='BLDG_AGE',predictor2=NULL,which2='mean',yrange=NULL,add=FALSE,div=100,Ncat=20)
{
  l <- getPredictorPlotCoordinates(full, mod, sps, predictor, predictor2, which2, yrange, add, div, Ncat)
  regression_coords <- l[[1]]
  occurence_coords <- l[[2]]
  
  p <- ggplot () +
    scale_x_continuous(name="") +
    scale_y_continuous(limits=c(0,1), name="Probability of occurence") +
    geom_line(data=regression_coords, aes(x=x, y=y)) +
    geom_point(data=occurence_coords, aes(x=x, y=y)) +
    #      theme(axis.title.x=element.blank()) +
    facet_wrap(~species, ncol=11)
  
  return (p)
}


graphOnePredictor=function(full=ctree,mod,sps='Acer saccharinum',predictor='BLDG_AGE',predictor2=NULL,which2='mean',yrange=NULL,add=FALSE,div=100,Ncat=20)
{
  
  regression_coords <- data.frame(x=numeric(), y=numeric(), species=character())
  occurrence_coords <- data.frame(x=numeric(), y=numeric(), species=character())
  
  for (sp in sps) {
    a <- graphOneResult(full,mod,sp,predictor, predictor2, which2, yrange, add, div, Ncat, retSpecs=TRUE)
    regression_coords <- rbind (regression_coords, data.frame(a[[1]], species=rep(sp, nrow(a[[1]]))))
    occurrence_coords <- rbind (occurrence_coords, data.frame(a[[2]], species=rep(sp, nrow(a[[2]]))))
  }

  p <- ggplot () +
    scale_x_continuous(name="") +
    scale_y_continuous(limits=c(0,1), name="Probability of occurence") +
    geom_line(data=regression_coords, aes(x=x, y=y)) +
    geom_point(data=occurrence_coords, aes(x=x, y=y)) +
    #      theme(axis.title.x=element.blank()) +
    facet_wrap(~species, ncol=11)
  
  return (p)
}


#OBSOLETE
getPredictorPlotCoordinates = function(full=ctree,mod,sps='Acer saccharinum',predictor='BLDG_AGE',predictor2=NULL,which2='mean',yrange=NULL,add=FALSE,div=100,Ncat=20)
{
    regression_coords <- matrix(, nrow=0, ncol=3, dimnames=list(NULL,c('x','y','species')))
    occurrence_coords <- matrix(, nrow=0, ncol=3, dimnames=list(NULL,c('x','y','species')))
    
        
    for (sp in sps) {
        a <- graphOneResult(full,mod,sp,predictor, predictor2, which2, yrange, add, div, Ncat, retSpecs=TRUE)
        
        regression_coords <- rbind (regression_coords, a[[1]])
        occurrence_coords <- rbind (occurrence_coords, a[[2]])
    }
    return (list(regression_coords, occurence_coords))
}


#DAM - remove default for predictor2
#DAM - add retSpecs input to return a list object with graph data instead of doing the plot
# Using model result from one species, show response graph vs. one variable. All others held constant at their means. Overlays points showing observed occurrence probability in a series of equal sized bins of the predictor (from graphOneResultBins). The argument mod is the result of modelAllSpecies. 
# The argument which2 can be set to 'mean','upper', or 'lower', and predictor2 can be included. Then the second predictor is set at either its mean, mean+2SD, or mean-2SD.
# This will work for any model result, but both predictor and predictor2 must be included in the model.
## Here is how to show how Fraxinus pennsylvanica varies with BLDG_AGE, overlaying curves with mean HU_DENS and 2SD above and below mean HU_DENS:
# graphOneResult(full=ctree,mod=fullmod,sp='Fraxinus pennsylvanica',predictor='BLDG_AGE',predictor2='HU_DENS',which2='mean')
# graphOneResult(full=ctree,mod=fullmod,sp='Fraxinus pennsylvanica',predictor='BLDG_AGE',predictor2='HU_DENS',which2='upper',add=TRUE)
# graphOneResult(full=ctree,mod=fullmod,sp='Fraxinus pennsylvanica',predictor='BLDG_AGE',predictor2='HU_DENS',which2='lower',add=TRUE)

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
 graphx=seq(min(x),max(x),len=div)	
 fullx=data.frame(graphx)
 
 colnames(fullx)=pred1
 for(onepred in allpredictors) if(onepred!=pred1) fullx[,onepred]=mean(full[,onepred])
 if (!is.null(pred2))
 {
    if(whichpred2=='upper') fullx[,pred2]=mean(full[,pred2])+2*sd(full[,pred2])
    else if(whichpred2=='lower') fullx[,pred2]=mean(full[,pred2])-2*sd(full[,pred2])
 }
	
 return(fullx)
}


# Using observations only, find a fitted probability by binning data into a set of equal quantiles for one predictor. The number of equal quantiles can be adjusted with the argument div.
graphOneResultBins=function(full=ctree,mod,sp='Acer saccharinum',predictor='BLDG_AGE',div=20,clr='red',add=FALSE, retSpecs=FALSE)
{
 spdata=prepareOneSpecies(full=full,s=sp)
 x=spdata[,predictor]
 
 bins=unique(quantile(x,prob=seq(0,1,len=div)))
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

# Calculate fitted probability for every species at every point. The requires fitting model for each species at full matrix of predictors. 
fullFittedMatrix=function(full=ctree,mod,allpredictors=c('BLDG_AGE','HU_DENS','DIST_WATER','HEIGHT_MAX','CROWN_AREA'))
{
 species=names(mod)
 Nsp=length(species)
 fullx=full[,allpredictors]
 result=matrix(ncol=Nsp,nrow=nrow(full))
 colnames(result)=species
 
 for(onesp in species)
  {
   cf=mod[[onesp]]$cf[,1]
   y=logistic.standard(x=fullx,param=cf)
   result[,onesp]=y
  }
  
 return(result)
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


