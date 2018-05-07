library("SparseFactorAnalysis")

## Not run:
##Sample size and dimensions.
set.seed(1)
n.sim<-50
k.sim<-500
##True vector of dimension weights.
d.sim<-rep(0,n.sim)
d.sim[1:3]<-c(2, 1.5, 1)*3
##Formulate true latent dimensions.
U.sim<-matrix(rnorm(n.sim^2,sd=.5), nr=n.sim, nc=n.sim)
V.sim<-matrix(rnorm(n.sim*k.sim,sd=.5), nr=k.sim, nc=n.sim)
Theta.sim<-U.sim%*%diag(d.sim)%*%t(V.sim)
##Generate binary outcome and count data.
probs.sim<-pnorm((-1+Theta.sim+rep(1,n.sim)%*%t(rnorm(k.sim,sd=.5)) +
                    rnorm(n.sim,sd=.5)%*%t(rep(1,k.sim))   ))
votes.mat<-
  apply(probs.sim[1:25,],c(1,2),FUN=function(x) rbinom(1,1,x))
count.mat<-
  apply(probs.sim[26:50, ],c(1,2),FUN=function(x) rpois(1,20*x))
M<-rbind(votes.mat,count.mat)
## Run sfa
sparse1<-sfa(M, maxdim=10)
##Analyze results.
summary(sparse1)
plot(sparse1,type="dim")
plot(sparse1,type="scatter")

plot(sparse1$Z,Theta.sim)
abline(c(0,1))
## End(Not run)