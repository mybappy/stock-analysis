library(quantmod)

# Convenience function for column names
cn <- function(s) {
    cndx.open  <- paste(s, ".Open", sep="")
    cndx.close <- paste(s, ".Close", sep="")
    cndx.adj   <- paste(s, ".Adjusted", sep="")
    cndx.o2c   <- paste(s, ".o2c", sep="")
    cndx.c2c   <- paste(s, ".c2c", sep="")
    list(open=cndx.open, close=cndx.close, adj=cndx.adj, o2c=cndx.o2c, c2c=cndx.c2c)
}

# Convenience method to turn the list into a return matrix
ret.mat <- function(slist, cndx) {
    shared.days <- slist[[1]]$day
    for(s in names(slist)) { shared.days <- intersect(shared.days, slist[[s]]$day) }
    
    .m <-
        do.call("cbind",
                lapply(names(slist),
                       function(s) {
                           cns <- cn(s)
                           .df <- slist[[s]]
                           return(.df[.df$day %in% shared.days, cns[[cndx]]])
                       }))
    rownames(.m) <- shared.days
    colnames(.m) <- names(slist)
    .m    
}

# Getting the data
yank.data <- function(symbols) {
    slist <- list()
    for(s in symbols) {
        cns <- cn(s)    
        xx <- getSymbols(s, auto.assign=F, src="yahoo", return.class="data.frame")
        days <- as.integer(strftime(strptime(rownames(xx), "%Y-%m-%d"), "%Y%m%d"))
        
        xx <- data.frame(day=days, xx[, c(cns$open, cns$close, cns$adj)])
        rownames(xx) <- seq(nrow(xx))
        slist[[s]] <- xx
    }
    slist
}

# Calculating returns (open to close, close to close)
calc.ret <- function(slist) {
    for(s in names(slist)) {
        cns <- cn(s)
        xx <- slist[[s]]
    
        df.o2c <- data.frame(day=xx$day, log(xx[, cns$close]) - log(xx[, cns$open]))
        names(df.o2c) <- c("day", cns$o2c)
        df.c2c <- data.frame(day=xx$day[-1], log(xx[-1, cns$adj]) - log(xx[-nrow(xx), cns$adj]))
        names(df.c2c) <- c("day", cns$c2c)
        df.all <- merge(df.o2c, df.c2c, by="day")
        slist[[s]] <- df.all
    }
    slist
}

symbols <- c("AAPL", "MSFT", "GOOG", "IBM", "T", "VZ", "CSCO", "ORCL", "QCOM", "INTC", "V", "MA", "EBAY",
             "EMC", "ACN", "TXN", "HPQ", "ADP", "YHOO", "CRM", "ADBE", "CTL", "CTSH", "GLW", "TEL")

slist <- yank.data(symbols)
slist <- calc.ret(slist)

o2c.ret <- ret.mat(slist, "o2c")
c2c.ret <- ret.mat(slist, "c2c")

#> head(o2c.ret[,1:3])
#                 AAPL         MSFT         GOOG
#20080320  0.016264211  0.015193662  0.014473983
#20080324  0.040365208 -0.005470099  0.049242976
#20080325  0.007261369 -0.006499082 -0.014710034
#20080326  0.029309971 -0.016322641  0.012297307
#20080327 -0.032962311 -0.015213455 -0.004314226
#20080328  0.008496944 -0.011400195 -0.021185606
#> 

#> head(cor(o2c.ret)[,1:3])
#          AAPL      MSFT      GOOG
#AAPL 1.0000000 0.5291187 0.6390242
#MSFT 0.5291187 1.0000000 0.5612872
#GOOG 0.6390242 0.5612872 1.0000000
#IBM  0.5701158 0.6092896 0.5607115
#T    0.4633655 0.5538030 0.5090261
#VZ   0.4129602 0.4929689 0.4639944
#>
