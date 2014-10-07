ge.lm2df <- function(lm) {
      #wrap a lm resuts to data frame format
      cf <- coef(lm)
      tinfo <- summary(lm)$coefficients[2, c(2, 4)]
      r2 <- summary(lm)$r.squared
      data.frame(intercept = cf[1], slope = cf[2], n = length(resid(lm)),
                 slope.se = tinfo[1], pval = tinfo[2], Rsq = r2)
    }
