## Show how to perform pairwise exact Wilcoxon rank sum tests, using
## the InsectSprays data set. I simply copy/pasted the
## pairwise.wilcox.test code, loaded exactRankTests and changed to the
## exact test.

pairwise.exact.wilcox.test <- function (x, g, p.adjust.method = p.adjust.methods, paired = FALSE, 
    ...) 
{
    library(exactRankTests)
    p.adjust.method <- match.arg(p.adjust.method)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    g <- factor(g)
    METHOD <- if (paired) 
        "Exact Wilcoxon signed rank test"
    else "Exact Wilcoxon rank sum test"
    compare.levels <- function(i, j) {
        xi <- x[as.integer(g) == i]
        xj <- x[as.integer(g) == j]
        wilcox.exact(xi, xj, paired = paired, ...)$p.value
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
        p.adjust.method = p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

## example usage
with(InsectSprays, pairwise.exact.wilcox.test(count, spray, "holm"))

## compare not exact variant
with(InsectSprays, pairwise.wilcox.test(count, spray, "holm"))
