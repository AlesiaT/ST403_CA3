df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


nll_lm <- function(data, par) {
  y <- data$y
  x <- data.matrix(data[,2:4])
  x <- cbind(1,x)
  n <- length(y)
  sigma <- par[1]
  beta <- par[2:5]
  first <- -n/2 * log(1/(2*pi*sigma^2))
  second <- -1/(2*sigma^2) * sum((y - x%*%beta)^2)
  log_lik <- first + second
  log_lik
}


inits <- c(1, mean(df$y), 0, 0, 0)
fit <- optim(inits, nll_lm, data=df, hessian=TRUE)
fit$par


y <- df$y
X <- data.matrix(df[,2:4])
X <- cbind(1, X)
beta <- solve(crossprod(X), crossprod(X, y))
beta


e <- y - X%*%beta
sigma_sq <- 1/(length(y) - 4) * sum(e^2)
sqrt(sigma_sq)


model <- lm(y ~ ., data = df)
model$coefficients
sigma(model)
