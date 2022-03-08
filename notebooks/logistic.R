sigma <- function(x, k=1) 1 / (1 + exp(-k * x))

gamma <- function(alpha_W, alpha_B) {
    return(1 - (sigma(alpha_W) * alpha_W + sigma(alpha_B) * alpha_B) /
           (sigma(alpha_W) + sigma(alpha_B)))
}

pred <- function(model, R_W, R_B) {
    coefs <- as.vector(model$coefficients)
    lin_term <- cbind(rep(1, length(R_W)), R_W, R_B) %*% coefs
    return(exp(-lin_term))
}

log_likelihood <- function(par, R_W, R_B) {
    intercept_W <- par[1]
    intercept_B <- par[2]
    weights_W <- par[3:(n_features + 2)]
    weights_B <- par[(n_features + 3) : (2 + n_features * 2)]

    alpha_W <<- sigma(intercept_W + white_features %*% weights_W)
    alpha_B <<- sigma(intercept_B + black_features %*% weights_B)

    R_W_adj <- R_W + 400 * log10(alpha_W)
    R_B_adj <- R_B + 400 * log10(alpha_B)

    t_w <- pred(w_model, R_W_adj, R_B_adj)
    t_w0 <- pred(w_model, R_W, R_B)
    t_d <- pred(d_model, R_W_adj, R_B_adj)
    p_w0 <<- t_w0 / (1 + t_w0 + t_d)
    p_w <<- t_w / (1 + t_w + t_d)
    p_d <<- t_d / (1 + t_w + t_d) + gamma(alpha_W, alpha_B) * (p_w - p_w0)
    p_l  <<- pmax(1 - p_d - p_w, rep(0, length(R_W))) # catch numerical errors

    wins <<- data$Result == "1-0"
    losses <<- data$Result == "0-1"
    draws <<- data$Result == "1/2-1/2"

    loglik <- sum(log(c(p_w[wins], p_d[draws], p_l[losses])))
    return(loglik)
}

parsim <- function(R_W, R_B) {
    t_w <- pred(w_model, R_W, R_B)
    t_d <- pred(d_model, R_W, R_B)
    p_w <<- t_w / (1 + t_w + t_d)
    p_d <<- t_d / (1 + t_w + t_d)
    p_l  <<- pmax(1 - p_d - p_w, rep(0, length(R_W))) # catch numerical errors

    wins <<- data$Result == "1-0"
    losses <<- data$Result == "0-1"
    draws <<- data$Result == "1/2-1/2"

    loglik <- sum(log(c(p_w[wins], p_d[draws], p_l[losses])))
    return(loglik)
}
data <- read.csv("../data/games_clean.csv")
data$WWin <- factor(data$Result == "1-0")
data$Draw <- factor(data$Result == "1/2-1/2")
data$Result <- factor(data$Result)
data$WhiteElo <- as.double(data$WhiteElo)
data$BlackElo <- as.double(data$BlackElo)

w_model <- glm(WWin ~ WhiteElo + BlackElo, data=data, family="binomial")
d_model <- glm(Draw ~ WhiteElo + BlackElo, data=data, family="binomial")

white_features <- scale(data.matrix(read.csv("../data/white_features.csv")))
black_features <- scale(data.matrix(read.csv("../data/black_features.csv")))

n_features <- dim(white_features)[2]

Rw <- data$WhiteElo
Rb <- data$BlackElo

opt <- optim(rep(0, 2 + n_features * 2), log_likelihood, R_W=Rw, R_B=Rb)

iW <- opt$par[1]
iB <- opt$par[2]
wW <- opt$par[3:(n_features + 2)]
wB <- opt$par[(n_features + 3) : (2 + n_features * 2)]

aW <- sigma(iW + white_features %*% wW)
aB <- sigma(iB + black_features %*% wB)
lr <- -2 * (opt$value - parsim(Rw, Rb))

threshold <- 0.999999
print(paste("Passes LR chi-sq test with threshold", threshold, ":", qchisq(threshold, 2*n_features + 2) < lr))

png("../paper/distributions.png", width=800, height=800, res=120)
plot(density(aW, bw=0.002), lwd=3, col="red", xlim=c(0,1), xlab="alpha",
     ylab="density", main="",
     ylim=c(0,18), cex.lab=1.2)
title(bquote("Estimated kernel density of " * alpha[W] * ", " * alpha[B]), cex.main=1.6)
lines(density(aB, bw=0.01), lwd=3, col="blue")
legend(0.75, 17, c(expression(alpha[W]), expression(alpha[B])), col=c("red", "blue"), lty=c(1,1), lwd=c(3,3), cex=1.2)
dev.off()

png("../paper/diff.png", width=800, height=800, res=120)
plot(density(aW-aB), lwd=3, col="red", xlab="difference in alpha",
     ylab="density", main="", cex.lab=1.2)
title(bquote("Estimated kernel density of " * alpha[W] - alpha[B]), cex.main=1.6)
dev.off()

aBmodel <- lm(aB ~ Rb)
x <- seq(2000, 3000)
y <- predict(aBmodel, list(Rb=x))
png("../paper/linear.png", width=1200, height=800, res=120)
plot(Rb, aB, pch=20, xlab="Rating", ylab=expression(alpha), cex.lab=1.2)
lines(x, y, lty=1, col="red", lwd=3)
title(bquote(alpha*" plotted against rating"), cex.main=1.6)
dev.off()
