#'	OneStageSS
#'
#'	Calculates sample size or power for single-stage trials with a binary endpoint according to:
#' 	Ahern, R. P., Sample size tables for exact single-stage phase II designs.
#'	Statistics in medicine 20.6 (2001): 859-866.
#'
#' @param p0 Proportion of events under H0
#' @param p1 Proportion of events under H1
#' @param power Power, default=0.8
#' @param n Sample size, calculated if NA, otherwise power is calculated
#' @param alpha Alpha, default=0.05
#' @param nstart Number of patients to start iterations, default=1, can be increased to save time
#'
#' @return List with number of patients with events for a significant trial (r), number of patients (n), 
#'		alpha and power.
#'
#' @importFrom stats pbinom
#'
#' @export
#'
#' @examples 
#' # Calculate sample size
#' OneStageSS(p0=0.5,p1=0.6,alpha=0.05,power=0.8)
#' # Calculate power
#' OneStageSS(p0=0.5,p1=0.6,alpha=0.05,n=100)
#'
OneStageSS<-function(p0,p1,power=0.8,n=NA,alpha=0.05,nstart=1) {
	
	if (is.na(n)) {
	ss<-numeric(0)
		n<-nstart-1
		while (length(ss)==0) {
			n<-n+1
			r<-1:n
			a<-1-pbinom(r-1,size=n,prob=p0)
			b<-pbinom(r-1,size=n,prob=p1)
			ss<-r[a<alpha & b<(1-power)]
		}
		res<-list(r=min(ss),n=n,alpha=a[min(ss)],power=1-b[min(ss)])
		return(res)
	} else {
		if (!is.na(power)) {
			warning("Sample size given, power is calculated")
		}
		r<-1:n
		a<-1-pbinom(r-1,size=n,prob=p0)
		b<-pbinom(r-1,size=n,prob=p1)
		if (sum(a<alpha)==0) {
			res<-list(r=NA,n=n,alpha=NA,power=NA)
		} else {
			res<-list(r=r[a==max(a[a<0.05])],n=n,alpha=max(a[a<0.05]),power=1-b[a==max(a[a<0.05])])
		}
		return(res)
	}
}
