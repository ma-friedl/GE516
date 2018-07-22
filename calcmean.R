calcmean=function(x) { 
	# calcmean is a function with input x
	n=length(x)	# get number of elements in x
	tot=sum(x)
	sd=var(x)^0.5		# compute sum
	return(c(tot/n,sd))	# divide by n!	
}
