utils::globalVariables(c("invDRIFT","II","DRIFTexp","vec2diag","diag2vec",
  "mxData","mxMatrix","mxAlgebra","MANIFESTVARbase","MANIFESTVARcholdiag",
  "MANIFESTVARchol","T0VARbase","T0VARcholdiag","T0VARchol","DIFFUSIONbase",
  "DIFFUSIONcholdiag","DIFFUSIONchol","invDRIFTHATCH","cvectorize","DRIFTHATCH",
  "TRAITVARbase","TRAITVARcholdiag","TRAITVARchol","MANIFESTTRAITVARbase",
  "MANIFESTTRAITVARcholdiag","MANIFESTTRAITVARchol","mxComputeSequence",
  "mxComputeGradientDescent","mxComputeReportDeriv","TDPREDVARbase",
  "TDPREDVARcholdiag","TDPREDVARchol","TIPREDVARbase","TIPREDVARcholdiag",
  "TIPREDVARchol","mxExpectationRAM","mxFitFunctionML","Ilatent","Alatent",
  "Amanifestcov","invIminusAlatent","Smanifest","Amanifest","Mmanifest",
  "mxExpectationNormal","omxSelectRowsAndCols","expCov","existenceVector",
  "omxSelectCols","expMean","log2pi","numVar_i","filteredExpCov","%&%",
  "filteredDataRow","filteredExpMean","firstHalfCalc","secondHalfCalc",
  "rowResults","mxFitFunctionRow","TdpredNames","discreteCINT_T1","discreteDRIFT_T1",
  "discreteDIFFUSION_T1","mxExpectationStateSpace","mxExpectationSSCT","ctsem.fitfunction",
  "ctsem.penalties","FIMLpenaltyweight","ctsem.simpleDynPenalty","ieigenval",
  "mxFitFunctionAlgebra","mxCI","mxComputeConfidenceInterval","DRIFT",
  "n.latent","DIFFUSION","TRAITVAR","n.TDpred","TDPREDEFFECT","TDPREDMEANS",
  "TDPREDVAR","TRAITTDPREDCOV","n.TIpred","TIPREDEFFECT","TIPREDMEANS",
  "TIPREDVAR","CINT","n.manifest","LAMBDA","MANIFESTMEANS","MANIFESTVAR",
  "mxFitFunctionMultigroup", "asymDIFFUSION", 'data.id',
  'filteredExpCovchol','filteredExpCovcholinv',
  'A','M','testd','ctstantestdat','smfnode',
  'T0VAR','T0MEANS', 'MANIFESTTRAITVAR',
  'TDpredNames', 'TIpredNames', 'Tpoints', 'extract', 'latentNames', 'manifestNames',
  'plot', 'points','T0TRAITEFFECT',
  'T0VARsubindex','DRIFTsubindex','DIFFUSIONsubindex','CINTsubindex','.'))

if(1==99){
  `:=` = NULL
`.` =NULL
.N = .id = id= . = grp = NULL # due to NSE notes in R CMD check
}


#' ctsemOMX
#' 
#' ctsem is an R package for continuous time structural equation modelling of panel (N > 1) 
#' and time series (N = 1) data, using either a frequentist or Bayesian approach, or middle
#' ground forms like maximum a posteriori. This ctsemOMX addition includes the original OpenMx based functions
#' which have been split off from the main package.
#' 
#' The general workflow begins by specifying a model using the \code{\link{ctModel}} function, 
#' in which the \code{type} of model is also specified. Then the model is fit to data using 
#' either \code{\link{ctFit}} if the original 'omx' (OpenMx, SEM, max likelihood) model is specified.
#' The omx forms are no longer in 
#' development and for most purposes, the newer stan based forms (contained in the base ctsem package)
#' are more robust and flexible.
#' For citation info, please run \code{citation('ctsem')} .
#'  
#' @docType package
#' @name ctsemOMX
#' @import ctsem
#' @import grDevices methods stats graphics OpenMx
#' @importFrom plyr aaply alply round_any
#' @importFrom utils relist as.relistable tail capture.output
#' 
#' @references 
#' https://www.jstatsoft.org/article/view/v077i05
#' 
#' Driver, C. C., & Voelkle, M. C. (2018). Hierarchical Bayesian continuous time dynamic modeling. 
#' Psychological Methods. Advance online publication.http://dx.doi.org/10.1037/met0000168
#' 
NULL

