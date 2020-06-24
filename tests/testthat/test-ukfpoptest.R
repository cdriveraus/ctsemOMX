if(identical(Sys.getenv("NOT_CRAN"), "true")){
  
  library(ctsem)
  library(testthat)
  
  context("ukfpopcheck") 
  
  test_that("ukfpopcheck1", {
    set.seed(1)
    Tpoints<-10
    n.latent=2
    n.manifest=3
    nsubjects=60
    burnin=30
    dtmat = matrix(exp(rnorm(burnin+Tpoints-1,-.3,.5)),1)
    par1<-rnorm(nsubjects,-.3,.8)
    par2 <- par1*.4 + rnorm(nsubjects,0,.3)
    for(i in 1:nsubjects){
      gm<-ctModel(type='omx',n.latent=2,n.manifest=n.manifest,Tpoints=Tpoints,LAMBDA=matrix(c(1,.4,0,0,0,1),3,ncol=2),
        DRIFT=diag(-.4,2),
        CINT=matrix(c(par1[i],par2[i]),2),
        T0VAR=diag(.1,2),
        T0MEANS=matrix(c(3,5),n.latent),
        # MANIFESTMEANS=diag(par1[i],1), #matrix(mmeans,nrow=n.manifest),
        MANIFESTVAR=t(chol(diag(.0001,n.manifest))),
        DIFFUSION=t(chol(diag(3,2))))
      if(i==1) cd<-ctGenerate(gm,n.subjects=1,burnin=burnin,wide=FALSE,dtmat = dtmat) else {
        newdat <- ctGenerate(gm,n.subjects=1,burnin=burnin,wide=FALSE,dtmat = dtmat)
        newdat[,'id'] <- i
        cd<-rbind(cd,newdat)
      }
    }
    
    cd[,gm$manifestNames]<-(cd[,gm$manifestNames]) + rnorm(length(cd[,gm$manifestNames]),0, .9^2)
    
    
    m1<-ctModel(type='omx',n.latent=4,n.manifest=3,Tpoints=Tpoints,
      LAMBDA=cbind(gm$LAMBDA,0,0),
      MANIFESTMEANS=matrix(0,nrow=n.manifest),
      # CINT=matrix(c('cint1',0),2,1),
      # T0MEANS=matrix(c('t0m1',0),2),
      # T0VAR=matrix(c('t0var11',0, 0,'t0var22'),2,2),
      # DIFFUSION=matrix(c('diff11',0,0,1e-5),2,2),
      # DRIFT=matrix(c('dr11',0,1,-1e-5),2,2)
    )
    m1$DRIFT[3:4,] <- 0
    m1$DRIFT[,3:4] <- 0
    diag(m1$DRIFT)[3:4] <- -1e-5
    m1$DRIFT[1,3] = 1
    m1$DRIFT[2,4] = 1
    m1$DIFFUSION[3:4,] <- 0
    m1$DIFFUSION[,3:4] <- 0
    # m1$T0VAR[3:4,1:2] <- 0
    
    #model for ukf ctsem
    m2<-ctModel(type='omx',n.latent=2,n.manifest=n.manifest,Tpoints=Tpoints,
      # T0MEANS=matrix(c(0),1),
      # T0VAR=matrix(c(1.5),1),
      MANIFESTMEANS=matrix(0,n.manifest),
      CINT=matrix(paste0('cint',1:gm$n.latent)),
      # DIFFUSION=matrix(c(1.4),1),
      # DRIFT=matrix(c(-.4),1),
      TRAITVAR='auto',
      LAMBDA=gm$LAMBDA
    )
    
    
    
    # #original ctsem
    cfit1 <- ctRefineTo(dat = cd,dataform = 'long',ctmodelobj = m1,retryattempts = 1)
    ct1d=cfit1$mxobj$DRIFT$values[1:2,1:2]
    # cfit1$mxobj$DIFFUSION$result
    # cfit1$mxobj$T0VAR$result
    ctll1=cfit1$mxobj$output$fit *-.5
    
    cfit2 <- ctRefineTo(dat = cd,dataform = 'long',ctmodelobj = m2,retryattempts = 0,carefulFit=T,stationary='')
    ct2d=cfit2$mxobj$DRIFT$values
    # cfit2$mxobj$DIFFUSION$result
    # cfit2$mxobj$T0VAR$result
    ctll2=cfit2$mxobj$output$fit *-.5
    
    # summary(cfit1)$ctparameters
    # summary(cfit2)$ctparameters
    
    #bayesian / ukf ctsem
    
    
    dvec=c('ct1d','ct2d')
    llvec=c('ctll1','ctll2')
    
    sapply(dvec,get,envir=sys.frame(sys.parent(0)))
    sapply(llvec,get,envir=sys.frame(sys.parent(0)))
    
    for(di in 2:length(dvec)){
      expect_equivalent(get(dvec[di]),get(dvec[di-1]),tol=1e-1)
      expect_equivalent(get(llvec[di]),get(llvec[di-1]),tol=1e-3)
    }
    
  })
  
  
}
