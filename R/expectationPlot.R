#' Plot a manager track record with ex-ante expection bounds.
#'
#' This function takes an xts object of manager returns, an expected ex-ante
#' Sharpe ratio vector and and ex-ante volatility vector and plots cone charts
#' for each of the managers. The plots have the expected growth trajectory as
#' well as one and two standard deviation bands. This helps detect whether
#' performance of the manager is progressing in line with ex-ante
#' expectations.
#'
#' @param x an xts object of returns.
#' @param exp.sharpe a vector of ex-ante Sharpe ratios for each of the assets.
#' @param exp.ann.vol a vector of annualized volatilities for each asset.
#' @param scale the annualization factor for the returns. (eg. 12 = monthly)
#' @param numrows the number of rows in the graphics window. Passed to mfrow().
#' @param numcols the number of columns in the graphics window. Passed to
#'   mfrow().
#' @param ... additional pass-through paramaters for chart.Timeseries
#'
#' @return a series of expectation plots for each asset in x.
#'
#' @export
#'
#' @examples
#' dts <- seq(Sys.Date()-19, Sys.Date(), 1)
#' returns <- matrix(rnorm(20),ncol = 1) / 100
#' ret.xts <- xts(returns, dts)
#' funds <- xts(matrix(rep(ret.xts,6),ncol = 6),index(ret.xts))
#' sharpes <- rep(1,6)
#' vols <- rep(.1,6)
#' expectationPlot(funds, sharpes, vols)

expectationPlot<-function(x, exp.sharpe, exp.ann.vol, scale = 12, cols = 1,
                          auto.arrange = TRUE, mytheme = theme_bw(base_size = 10)){

  if (!auto.arrange){
    grid.newpage()
    layout <- matrix(seq(1, cols * ceiling(ncol(x)/cols)),
                     ncol = cols, nrow = ceiling(ncol(x)/cols))
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  }

  if (! is.xts(x)){
    stop("Must supply an xts object.")
  } else {

    theplots <- list()
    for (i in 1:ncol(x)){
      exp.ret <- exp.sharpe[i]*exp.ann.vol[i]
      exp.vol.monthly <- exp.ann.vol[i]/sqrt(scale)

      exp.track <- xts(rep(exp.ret/scale,nrow(x)),index(x))
      exp.track.cum <- cumsum(exp.track)
      dt <- seq(1:length(exp.track))

      upsd <- NULL # std deviation in up state
      upsd[1] <- 0
      for (j in 2:length(exp.track)){
        upsd[j] <- exp.vol.monthly*sqrt(dt[j])
      }

      upsd <- xts(upsd,index(exp.track))
      manager <- x[,i]
      manager[1,] <- 0
      manager <- cumsum(manager)
      thename <- colnames(x[,i])

      plotdata <- data.frame(index(manager),coredata(manager),
                             coredata(exp.track.cum),
                             upsd+coredata(exp.track.cum),
                             coredata(exp.track.cum)-upsd,
                             coredata(exp.track.cum)+2*upsd,
                             coredata(exp.track.cum)-2*upsd)

      names(plotdata) <- c("date","manager","exp.track","one_sdup",
                           "one_sddown","two_sdup","two_sddown")
      plotdata[1,2:ncol(plotdata)] <- 0

      theplots[[i]] <- ggplot(plotdata, aes(x = date)) +
        geom_line(aes(y = manager), size=1.0, colour="black")+
        geom_line(aes(y = exp.track), size=1.0, colour="green") +
        geom_line(aes(y = one_sdup), size = 1.0, colour = "grey")+
        geom_line(aes(y = one_sddown), size = 1.0, colour = "grey")+
        geom_line(aes(y = two_sdup), size = 1.0, colour = "grey")+
        geom_line(aes(y = two_sddown), size = 1.0, colour = "grey")+
        ylab("Cumulative")+
        xlab("")+
        mytheme +
        ggtitle(paste(thename))

      # cleanup...
      rm(plotdata, manager, exp.track.cum, upsd, exp.track,
         exp.ret, exp.vol.monthly)
    }
    if (auto.arrange){
      do.call(marrangeGrob, args = list(theplots, ncol = ifelse(ncol(x) >= 6, 3, 2),
                                        nrow = ifelse(ncol(x) >=6, 3, 2)))
    } else {
      for (i in 1:length(theplots)){
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(theplots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                           layout.pos.col = matchidx$col))
      }
    }
  }
}
