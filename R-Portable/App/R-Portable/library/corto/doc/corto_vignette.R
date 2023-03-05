## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#"
)

## ----install, eval=FALSE------------------------------------------------------
#  install.packages("corto")

## ----load---------------------------------------------------------------------
library(corto)

## ----load1--------------------------------------------------------------------
load(system.file("extdata","inmat.rda",package="corto"))
inmat[1:5,1:5]

## ----load2--------------------------------------------------------------------
dim(inmat)

## ----load3--------------------------------------------------------------------
load(system.file("extdata","centroids.rda",package="corto"))
centroids[15]

## ----load4--------------------------------------------------------------------
length(centroids)

## ----runcorto,message=FALSE,results="hide"------------------------------------
regulon<-corto(inmat,centroids=centroids,nbootstraps=10,p=1e-30,nthreads=2)
# Input Matrix has 87 samples and 10021 features
# Correlation Coefficient Threshold is: 0.889962633618839
# Removed 112 features with zero variance
# Calculating pairwise correlations
# Initial testing of triplets for DPI
# 246 edges passed the initial threshold
# Building DPI network from 37 centroids and 136 targets
# Running 100 bootstraps with 2 thread(s)
# Calculating edge likelihood
# Generating regulon object

## ----prinregulon--------------------------------------------------------------
regulon[1:2]

## ----prinregulon2-------------------------------------------------------------
length(regulon)

## ----prinregulon3-------------------------------------------------------------
names(regulon)

## ----runcnv, eval=FALSE-------------------------------------------------------
#  load(system.file("extdata","cnvmat.rda",package="corto",mustWork=TRUE))
#  regulon <- corto(inmat,centroids=centroids,nthreads=2,nbootstraps=10,verbose=TRUE,cnvmat=cnvmat,p=0.01)

