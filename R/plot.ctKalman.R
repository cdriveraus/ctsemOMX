#' Plots Kalman filter output from ctKalman.
#'
#' @param x Output from \code{\link{ctKalman}}. In general it is easier to call 
#' \code{\link{ctKalman}} directly with the \code{plot=TRUE} argument, which calls this function.
#' @param subjects vector of integers denoting which subjects (from 1 to N) to plot predictions for. 
#' @param kalmanvec string vector of names of any elements of the output you wish to plot, 
#' the defaults of 'y' and 'yprior' plot the original data, 'y', 
#' and the prior from the Kalman filter for y. Replacing 'y' by 'eta' will 
#' plot latent variables instead (though 'eta' alone does not exist) and replacing 'prior' 
#' with 'upd' or 'smooth' respectively plotting updated (conditional on all data up to current time point)
#' or smoothed (conditional on all data) estimates.
#' @param errorvec vector of names of covariance elements to use for uncertainty indication 
#' around the kalmanvec items. 'auto' uses the latent covariance when plotting
#' latent states, and total covariance when plotting expectations of observed states. 
#' Use NA to skip uncertainty plotting.
#' @param errormultiply Numeric denoting the multiplication factor of the std deviation of errorvec objects. 
#' Defaults to 1.96, for 95\% intervals.
#' @param ltyvec vector of line types, varying over dimensions of the kalmanvec object.
#' @param colvec color vector, varying either over subject if multiple subjects, or otherwise over 
#' the dimensions of the kalmanvec object.
#' @param lwdvec vector of line widths, varying over the kalmanvec objects. 
#' @param subsetindices Either NULL, or vector of integers to use for subsetting the (columns) of kalmanvec objects.
#' @param pchvec vector of symbol types, varying over the dimensions of the kalmanvec object.
#' @param typevec vector of plot types, varying over the kalmanvec objects. 'auto' plots lines for
#' any  'prior', 'upd', or 'smooth' objects, and points otherwise.
#' @param grid Logical. Plot a grid?
#' @param add Logical. Create a new plot or update existing plot?
#' @param plotcontrol List of graphical arguments (see \code{\link{par}}), 
#' though lty,col,lwd,x,y, will all be ignored.
#' @param legend Logical, whether to include a legend if plotting.
#' @param legendcontrol List of arguments to the \code{\link{legend}} function.
#' @param polygoncontrol List of arguments to the \code{\link{ctPoly}} function for filling the uncertainty region.
#' @param polygonalpha Numeric for the opacity of the uncertainty region.
#' @param ... not used.
#' @return Generates plots.
#' @method plot ctKalman
#' @export
#' @examples
#' \donttest{
#' data(AnomAuth) 
#' AnomAuthmodel <- ctModel(LAMBDA = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2), 
#'   Tpoints = 5, n.latent = 2, n.manifest = 2) 
#' AnomAuthfit <- ctFit(AnomAuth, AnomAuthmodel)
#' ctKalman(AnomAuthfit,subjects=1,plot=TRUE)
#' }
plot.ctKalman<-function(x, subjects=1, kalmanvec=c('y','yprior'),
  errorvec='auto', errormultiply=1.96,
  ltyvec="auto",colvec='auto', lwdvec='auto', 
  subsetindices=NULL,pchvec='auto', typevec='auto',grid=FALSE,add=FALSE, 
  plotcontrol=list(ylab='Value',xlab='Time',xaxs='i',lwd=2,mgp=c(2,.8,0)),
  polygoncontrol=list(steps=20),polygonalpha=.1,
  legend=TRUE, legendcontrol=list(x='topright',bg='white',cex=.7),...){
  
  if(!'ctKalman' %in% class(x)) stop('not a ctKalman object')
  
  
  
  if('ctKalman' %in% class(x)){ #base plots
    if(length(subjects) > 1 & colvec[1] =='auto') colvec = rainbow(length(subjects),v=.9)
    
    if(lwdvec[1] %in% 'auto') lwdvec=rep(2,length(kalmanvec))
    
    if(is.null(plotcontrol$ylab)) plotcontrol$ylab='Variable'
    if(is.null(plotcontrol$xlab)) plotcontrol$xlab='Time'
    
    if(typevec[1] %in% 'auto') typevec=c('p','l')[grepl("prior|upd|smooth|eta",kalmanvec)+1]
    
    if(errorvec[1] %in% 'auto') {
      errorvec=rep(NA,length(kalmanvec))
      errorvec[grepl("prior|upd|smooth",kalmanvec)]<-paste0(
        kalmanvec[grepl("prior|upd|smooth",kalmanvec)],'cov')
    }
    
    if(is.null(plotcontrol$xlim)) plotcontrol$xlim <- range(sapply(x,function(x) x$time))
    if(is.null(plotcontrol$ylim)) {
      plotcontrol$ylim <- range(unlist(lapply(x,function(x) { #for every subject
        if(!is.null(x)){
          ret<-c()
          for(kveci in 1:length(kalmanvec)){
            est<-x[[kalmanvec[kveci]]] [,
              if(is.null(subsetindices)) 1:dim(x[[kalmanvec[kveci]]])[2] else subsetindices]
            
            if(!is.na(errorvec[kveci])) err <- sqrt(abs(c(apply(x[[errorvec[kveci]]][
              (if(is.null(subsetindices)) 1:dim(x[[errorvec[kveci]]])[2] else subsetindices),
              (if(is.null(subsetindices)) 1:dim(x[[errorvec[kveci]]])[2] else subsetindices),
              ,drop=FALSE],3,diag))))
            
            if(is.na(errorvec[kveci])) err <- 0
            
            esthigh <- est + err*errormultiply
            estlow <- est - err*errormultiply
            ret <- c(ret,esthigh,estlow)
          }
          return(ret)}})),na.rm=TRUE)
      if(legend) plotcontrol$ylim[2] <- plotcontrol$ylim[2] + sd(plotcontrol$ylim)/4
    }
    
    
    
    
    legendtext<-c()
    legendcol <- c()
    legendlty<-c()
    legendpch<-c()
    
    #when not set to auto, must define 'new' vector as the user specified vector
    colvecnew <- colvec
    ltyvecnew<-ltyvec
    pchvecnew <- pchvec
    
    for(si in 1:length(subjects)){#subjects
      subjecti = subjects[si]
      subiname=paste('subject',subjecti)
      names(x)[si] <-subiname
      plist<-plotcontrol
      if(length(subjects) > 1) {
        plist$col = colvec[si] #set colour based on subject if multiple subjects
      }
      
      for(kveci in 1:length(kalmanvec)){ #kalman output types
        kvecdims=1:dim(x[[subiname]][[kalmanvec[kveci]]])[-1]
        if(length(subjects) == 1 & colvec[1] =='auto') colvecnew = rainbow(length(kvecdims),v=.9)
        if(any(subsetindices > max(kvecdims))) stop('subsetindices contains a value greater than relevant dimensions of object in kalmanvec!')
        if(!is.null(subsetindices)) kvecdims=kvecdims[subsetindices]
        if(rl(ltyvec[1]=='auto')) ltyvecnew <- 1:length(kvecdims) else ltyvecnew <- ltyvec
        
        if(rl(pchvec[1] =='auto')) pchvecnew = 1:(length(kvecdims)) else pchvecnew <- pchvec
        
        for(dimi in 1:length(kvecdims)){ #dimensions of kalman matrix
          kdimi <- kvecdims[dimi]
          plist$x=x[[subiname]]$time
          plist$y=x[[subiname]][[kalmanvec[kveci]]][,kdimi] 
          plist$lwd=lwdvec[kveci]
          plist$lty=ltyvecnew[dimi] 
          plist$pch=pchvecnew[dimi]
          plist$type=typevec[kveci]
          if(length(subjects)==1) plist$col=colvecnew[dimi]
          
          
          if(subjecti == subjects[1] & kveci==1 && dimi == 1 && !add) {
            
            do.call(graphics::plot.default,plist) 
            if(grid) {
              grid()
              par(new=TRUE)
              do.call(graphics::plot.default,plist) 
              par(new=FALSE)
            }
          } else do.call(graphics::points.default,plist) 
          
          if(!is.na(errorvec[kveci])){
            if(is.null(x[[subiname]][[errorvec[kveci]]])) stop('Invalid errorvec specified!')
            backwardstimesindex=order(plist$x,decreasing=TRUE)
            
            ctpolyargs<-polygoncontrol
            ctpolyargs$x=c(plist$x)
            ctpolyargs$ylow=c(plist$y - errormultiply * sqrt(abs(x[[subiname]][[errorvec[kveci]]][kdimi,kdimi,])))
            ctpolyargs$y=c(plist$y)
            ctpolyargs$yhigh=c(plist$y + errormultiply * sqrt(abs(x[[subiname]][[errorvec[kveci]]][kdimi,kdimi,])))
            ctpolyargs$col=grDevices::adjustcolor(plist$col,alpha.f=polygonalpha)
            ctpolyargs$col =grDevices::adjustcolor(ctpolyargs$col,alpha.f=max(c(.004,polygonalpha/sqrt(ctpolyargs$steps))))
            do.call(ctPoly,ctpolyargs)
            
            #add quantile lines
            plist$y <- ctpolyargs$ylow
            plist$lwd <- 1
            # plist$col <- grDevices::adjustcolor(plist$col,alpha.f=.5)
            do.call(points,plist)
            plist$y <- ctpolyargs$yhigh
            do.call(points,plist)
          }
          
          #if changing lty then legend needs lty types
          if(subjecti == subjects[1]) { #length(unique(ltyvecnew))>1 && 
            legendtext<-c(legendtext,paste0(kalmanvec[kveci],': ',
              colnames(x[[subiname]][[kalmanvec[kveci]]])[kdimi]))
            legendlty <- c(legendlty,ifelse(plist$type=='p',0,ltyvecnew[dimi]))
            legendpch <- c(legendpch,ifelse(plist$type=='l',NA,pchvecnew[dimi]))
            if(length(subjects) == 1) legendcol = c(legendcol,plist$col) else legendcol=c(legendcol,'black')
          }
        }
      }
    }
    
    if(length(subjects) > 1 && length(unique(colvec))>1) { #include subject color in legend if necessary
      legendtext<-c(legendtext,paste('Subject', subjects))
      legendcol <- c(legendcol,colvec)
      legendlty <-c(legendlty,rep(0,length(subjects)))
      legendpch <-c(legendpch,rep(NA,length(subjects)))
    }
    
    if(legend && length(legendtext)>0){
      legendcontrol$legend<-legendtext
      legendcontrol$col<-legendcol
      legendcontrol$text.col <- legendcol
      legendcontrol$pch <- legendpch
      legendcontrol$lty <- legendlty  
      do.call(graphics::legend,legendcontrol)
    }
  }#end base plots
}
