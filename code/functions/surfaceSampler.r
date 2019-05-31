#' @title surfaceSampler
#' @description Creates a simulated P/A data set of a mystery species
#' @export

surfaceSampler <- function(x,polygon,nSamples,setMult = 1, nIter = 1 ) {
	
	#X = distretized surface : names(X,Y,Prob)
	#polygon = polygons to sample from: names = X,Y,PID,POS
	#nSamples = data.frame(PID,SamplesPerPID)
	#nIter = number of iterations to get samples
	require(PBSmapping)
	
	pip = findPolys(x,polygon,includeBdry=1,maxRows = nrow(x))
	x = merge(x,pip[,1:2],by='EID')
	x$PID[which(x$PID==444 & x$z<91)] <-443
	x$PID[which(x$PID==444 & x$z>181)] <-445

	pO = unique(x$PID)
	out <- c()
	for(i in 1:length(pO)) {
		pS = subset(nSamples, PID == pO[i],select='Nsets') * setMult
		g  = subset(x,PID == pO[i])
		sS = g[sample(1:nrow(g),size=pS$Nsets),]
		sS$Pres <- NA
			for(j in 1:nrow(sS)) {
				sS$Pres[j] = rbinom(1,size=1,prob=sS$habitat.mean[j])	
				}
		#sS$habitat.mean <- NULL
		out = rbind(out,sS)
		}
		names(out) <- c('setid','Depth','Temperature','X','Y','H.V','Strata','P.A')
		return(out)
}