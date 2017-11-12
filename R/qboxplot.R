qboxplot = function(x, range=1.5, probs=c(0.25,0.5,0.75), qtype=7, data=parent.frame(),
                    width=NULL, varwidth=FALSE, outline=TRUE, names=NULL, plot=TRUE,
                    border=par("fg"), col=NULL, log="", pars=list(boxwex=0.8, staplewex=0.5,
                    outwex=0.5), horizontal=FALSE, add=FALSE, at=NULL, ...) {

	if(is.data.frame(x)) {
		data = stack(x)
		formula = formula(values ~ ind)
	}
	if(is(x,"formula")) {
		formula = x
	}

	#get quantiles
	probs = sort(probs)
	quant = aggregate(formula, data=data, FUN=qboxplot.stats, probs=probs, qtype=qtype,
                          range=range, output="quantiles")

	#get names
	n = length(quant)
	labels = quant[,1:(n-1)]
	if(is.null(names)) {
		if(n == 2) {
			names = as.character(labels)
		} else {
			names = as.character(labels[,1])
			for(i in 2:(n-1)) {
				names = paste(names, as.character(labels[,i]), sep=".")
			}
		}
	}

	#get outliers
	out = numeric(0)
	group = numeric(0)
	outliers = aggregate(formula, data=data, FUN=qboxplot.stats, probs=probs, qtype=qtype,
                             range=range, output="outliers")
	for (i in 1:nrow(outliers)) {
	  out = c(out, unlist(outliers[i, n]))
	  group = c(group, rep(i, length(unlist(outliers[i, n]))))
	}

	#get sample size
	ng = aggregate(formula, data=data, FUN=qboxplot.stats, probs=probs, qtype=qtype,
                             range=range, output="n")

	#plot and/or return data
	bxpdata = list(stats=t(quant[,n]), n=ng[,n], out=out, group=group, names=names)
	args = list(formula, ...)
	if(plot) {
		if(is.null(pars$boxfill) && is.null(args$boxfill)) {
			pars$boxfill = col
		}
		do.call("bxp", c(list(bxpdata, notch=FALSE, width=width, varwidth=varwidth, log=log,
		                      border=border, pars=pars, outline=outline, horizontal=horizontal,
		                      add=add, at=at), args))
		invisible(bxpdata)
	} else {
		return(bxpdata)
	}

}
