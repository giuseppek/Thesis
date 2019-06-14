
means= rep(0,1000)
medians= rep(0, 1000)
geometrical_means= rep(0, 1000)
for(i in 1:1000){ means[i]= mean(rpois(10,1000) +  rnorm(10,0, 1000))
geometrical_means[i] = mean( log( rpois(10,1000)+ rnorm(10,0, 1000) ))
medians[i]= mean( rpois(10,1000) + rnorm(10,0, 1000), trim= 0.5)
}
var(means)
var(medians)
var(exp(geometrical_means))

mean(means)
mean(medians)
mean(exp(geometrical_means))

p= 0.995 
#library(pracma)
#var.test(means,exp(geometrical_means), alternative = "two.sided")
#var.test(means,medians, alternative = "two.sided")
a = matrix( ncol = 150, nrow = 3000)
for(k in 1:150){
for(i in 1:3000){
a[i,k] = mean( rpois(5+k,800) +(runif(5+k)>p)* rpois(5+k,300)    )  }}


ts.plot( ts(apply( a, 2,var) , start= 6 )[1:100])

b = matrix( ncol = 150, nrow = 3000)
for(k in 1:150){
  for(i in 1:3000){
    b[i,k] = median( rpois(5+k,800) + (runif(5+k)>p )* rpois(5+k,200)  )  }}

ts.plot( ts(apply( b, 2,var) , start= 6 ) ,     ts(apply( a, 2,var) , start= 6 ), col= c(2,1)  )

c = matrix( ncol = 150, nrow = 3000)
for(k in 1:150){
  for(i in 1:3000){
    c[i,k] =  geomean( rpois(5+k,800) + (runif(5+k)>p)* rpois(5+k,200)  ) }}

ts.plot( ts(apply( b, 2,std) , start= 6 ) ,     ts(apply( a, 2,std) , start= 6 ), ts(apply( c, 2,std) , start= 6 ) ,   col= c(2,1,3) ,  gpars=list( main= "Scenario with 'hardly detectable' outliers",xlab="Sample size", ylab="Standard deviation") )
legend(115, sqrt(200), legend=c("Median", "Mean", "Geometric Mean"), col=c(2,1,3), lty=1, cex=1)


#ts.plot( ts(apply( f, 2,std) , start= 6 ) ,     ts(apply( e, 2,std) , start= 6 ), ts(apply( g, 2,std) , start= 6 ) ,   col= c(2,1,3) ,  gpars=list( main= "Scenario with outliers",xlab="Sample size", ylab="Standard deviation") )
#legend(115, 25, legend=c("Median", "Mean", "Geometric Mean"), col=c(2,1,3), lty=1, cex=1)

#ts.plot( ts(apply( h, 2,std) , start= 6 ) ,     ts(apply( i, 2,std) , start= 6 ), ts(apply( l, 2,std) , start= 6 ) ,   col= c(2,1,3) ,  gpars=list( main= "Scenario with no outliers",xlab="Sample size", ylab="Standard deviation") )
#legend(115,13, legend=c("Median", "Mean", "Geometric Mean"), col=c(2,1,3), lty=1, cex=1)


#the simulation shows how the geometric mean is efficient as the mean in case of no outliers, and how it is still robust in presence of outlier
# considering the fact that a pre-process and outlier detection will be performed, and the multiplicative 
# assumption of call volumes data generating precess, the geometric mean seems a reasonable apprach



