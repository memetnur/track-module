## Compare power of Friedman test and LMM
##
## Errors and random intercepts are simulated as normally distributed 
##
library(nlme)
## Arguments:
## =========
## n.block      : number of blocks
## n.treatments : number of treatments
## means        : vector of treatment means
## sds          : vector of treatments standard deviations
## sd.block     : the sd of the random block effect
## runs         : number of runs
## sig.level    : significance level

## Simulate data
compare.sim <- function(n.block, n.treatments, means, sds, sd.block, runs, sig.level = 0.05) {
    df <- cbind(block = paste0("B", rep(1:n.block, each = n.treatments)),
                treatment = paste0("T", rep(1:n.treatments, each = 1)))
    df <- data.frame(df)
    p.val <- data.frame(matrix(NA, nrow = runs, ncol = 2))
    names(p.val) <- c("Friedman.p.value", "Mixed.model.pvalue")
    sig <- function(x) length(x[x < sig.level]) / length(x)
    for (i in 1: dim(p.val)[1]) {
        df$y <- rnorm(n = dim(df)[1],
                      mean = rep(rnorm(n = n.block, mean = 0, sd = sd.block), each = n.treatments) +
                          rep(rep(means), times = n.block),
                      sd = rep(rep(sds), times = n.block))
        p.val[i, ] <- c(ftp = friedman.test(y ~ treatment | block, data = df)$p.value,
                        lmp = anova(lme(y ~ treatment, random = ~ 1 | block, data=df))$`p-value`[2])
    }
    return(apply(p.val, MARGIN = 2, sig))
}


## Application
set.seed(2)

for (j in 1:2){
    print(compare.sim(n.block = 4, n.treatments = 3,
            means = c(1, 1.5, 2), sds = c(1, 1, 1), sd.block = 1.5,
            runs = 1000))
}
