df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


nll_lm <- function(data, par) {
  y <- data$y
  x <- cbind(1,data.matrix(data[,2:4]))
  sigma <- par[1]
  beta <- par[2:5]
  sum((y - x%*%beta)^2)
}


inits <- c(1, mean(df$y), 0, 0, 0)
fit <- optim(inits, nll_lm, data=df, hessian=TRUE)
options(scipen=999)
fit$par


y <- df$y
X <- data.matrix(df[,2:4])
X <- cbind(1, X)
beta <- solve(crossprod(X), crossprod(X, y))
beta
all.equal(fit$par[-1], as.numeric(beta))


e <- y - X%*%beta
sigma_sq <- 1/(length(y) - 4) * sum(e^2)
sqrt(sigma_sq)
all.equal(fit$par[1], sqrt(sigma_sq))


model <- lm(y ~ ., data = df)
model$coefficients
sigma(model)
