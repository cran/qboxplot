qboxplot.stats = function(x, probs, qtype, range, output="all") {
	x = na.omit(x)
	n = length(x)
	quantiles = quantile(x, probs=probs, names=FALSE, type=qtype)
	if(range == 0) {
		quantiles = c(min(x), quantiles, max(x))
		outliers = numeric(0)
	} else {
		iqr = quantiles[3] - quantiles[1]
		rmin = min(c(x[x>=quantiles[1]-range*iqr]),quantiles[1])
		rmax = max(c(x[x<=quantiles[3]+range*iqr],quantiles[3]))
		quantiles = c(rmin, quantiles, rmax)
		outliers = x[x<rmin|x>rmax]
	}
	match = pmatch(output, c("quantiles", "outliers", "n", "all"))
	if(match==1) {
		return(quantiles)
	} else if(match==2) {
		return(outliers)
	} else if(match==3) {
		return(n)
	} else {
		return(list(quantiles=quantiles, outliers=outliers, n=n))
	}
}
