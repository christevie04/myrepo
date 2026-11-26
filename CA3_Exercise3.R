#3.1

df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
head(df)

#3.2

nll_lm <- function(data,par){
  mu <- par[1] + par[2]*data$x1 + par[3]*data$x2+par[4]*data$x3
  eps <- data$y - mu
  -sum(dnorm(eps, mean = 0, sd= par[5], log = TRUE ))
}

#3.3

par0 <- c(mean(df$y), 0,0,0, sd(df$y))
nll_lm(df,par0)

opt <- optim( par = par0, fn=nll_lm, data = df, method = "L-BFGS-B", lower = c(-Inf,-Inf,-Inf,-Inf,1e-6), upper= rep(Inf,5))
opt$par
opt$convergence
#3.4
#we use the negative log likelihood because optim() finds parameter values that minimize a function and maximum likelihood estimation finds parameter value that maximizes the likelihood.

#3.5

X <- cbind(1,as.matrix(df[,c("x1","x2","x3")]))
Y <- df$y
betah <- solve(t(X)%*%X)%*%t(X)%*%Y
betah

#3.6

rd <- Y - as.vector(X%*%betah)
sigmah <- sqrt(sum(rd^2)/(nrow(X)- ncol(X)))
sigmah
opt$par[5]

#3.7
#the  denominator used  estimate the residual variance, the matrix calc uses a smaller number n - p = 28, which gives a larger estimate than dividing by 32
#3.8

opt <- optim( par = par0, fn=nll_lm, data = df, method = "L-BFGS-B", lower = c(-Inf,-Inf,-Inf,-Inf,1e-6), upper= rep(Inf,5), hessian = TRUE)

opt$convergence
dim(opt$hessian)

se <- sqrt(diag(solve(opt$hessian)))
se[1:4]
