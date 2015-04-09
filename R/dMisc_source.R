# Source file for utility and helper functions I've created over the years.
# Adam Duncan, 2015

# dMisc

makeXTS <- function(x, format = "%Y-%m-%d",
                    names = colnames(x)[2:ncol(x)]) {
  # Takes a data frame from read.csv, extracts the date column, and converts
  # the data frame to an xts object. First column of object should be 'Date'
  # and contain the dates.

  dts <- as.Date(x[, 1], format = format)
  data <- x[, 2:ncol(x)]  # remove the date column
  out <- xts(data, dts)  # form the xts object
  names(out) <- names
  return(out)
}
xtsApply <- function(x, cFUN, margin = 2, ...) {
  # an implementation of apply that works for xts objects...
  if (!is.xts(x))
    stop("Must supply function with xts object")

  if (margin == 2) {
    # column-wise calculation
    Z <- x
    for (j in 1:ncol(x)) {
      Z[, j] <- do.call(cFUN, list(x[, j], ...))
    }
  } else {
    # row-wise calculation
    Z <- xts(rep(0, nrow(x)), index(x))
    for (j in 1:nrow(x)) {
      Z[j, 1] <- do.call(cFUN, list(x[j, ], ...))
    }
  }
  return(Z)
}
xtsToDataFrame <- function(xts.obj) {
  # A function to convert an xts object into a data frame that can be plotted
  # usinfg ggplot2. The date column will be the first column of the new data
  # frame and will have the name 'Date'.  This function uses a recursive
  # implementation for xts objects with many columns.

  if (ncol(xts.obj) == 1) {
    series.name <- colnames(xts.obj)[1]
    tmp <- xts.obj
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df, id.var = "Date")
    tmp.df.long$asset <- rep(series.name, nrow(tmp.df.long))
    return(tmp.df.long)
  } else {
    num.assets <- ncol(xts.obj)
    asset.names <- colnames(xts.obj)
    df <- do.call(rbind, lapply(1:num.assets, function(x) {
      xtsToDataFrame(as.xts(xts.obj[, x]))
    }))
    df$asset <- ordered(df$asset, levels = asset.names)
    return(df)
  }
}
xtsAlign <- function(target, match.to, join = "inner") {
  # Takes two xts objects and aligns their dates returning target with the
  # same date structure as match.to (trims does not merge objects)

  dummy <- xts(rep(1, nrow(match.to)), index(match.to))
  ret <- merge.xts(target, dummy, join = join)
  ret <- subset(ret, select = (-dummy))
  rm(dummy)
  return(ret)
}
xtsMultiMerge <- function(xtslst, join = "inner") {
  # A hack to allow merging multiple xts objects AND apply the desired database
  # join. Default is 'inner' join.
  out <- NULL
  for (i in 1:length(xtslst)) {
    out <- merge.xts(out, xtslst[[i]], join = "inner")
  }
  return(out)
}
makeIndex <- function(x, inv = FALSE, ret = TRUE, na.rm = TRUE) {
  # Takes an xts object x and returns an index starting at 100 and evolving
  # as the arithmetic returns of x.  The inv flag tells whether or not to
  # invert the series before calculating returns.  The ret flag tells whether
  # or not we have been passed a series of returns already.
  x <- as.xts(x)
  init.val <- 100
  nam <- names(x)
  dts <- as.Date(unlist(strsplit(as.character(index(x)), " ")))
  if (na.rm) {
    data <- apply(x, 2, na.omit)
  }
  if (inv == TRUE)
    data <- 1/x else data <- x
    if (ret == TRUE) {
      # we have a series of returns...
      ret.series <- data
    } else {
      ret.series <- Return.calculate(data, method = "discrete")
    }
    n <- nrow(ret.series)
    new.series <- rep(0, n)
    new.series[1] <- init.val

    for (i in 2:n) {
      new.series[i] <- (1 + ret.series[i]) * new.series[i - 1]
    }
    new.series <- xts(new.series, dts)
    names(new.series) <- nam
    return(new.series)
}
indexToXTSReturns <- function(x) {
  # takes a data frame of wealth indices and returns an xts object of returns.

  if (names(x)[1] == "Date") {
    dts <- as.Date(x$Date, format = "%Y-%m-%d")
    ret <- subset(x, select = -Date)
    ret <- xts(ret, dts)
    ret.ret <- apply(ret, MARGIN = 2, Return.calculate, method = "log")
  } else {
    dts <- index(x, 0)
    # we already have an xts object
    ret.ret <- apply(x, MARGIN = 2, Return.calculate, method = "log")
  }
  ret.ret[1, ] <- 0
  ret.ret <- xts(ret.ret, dts)
  return(ret.ret)
}
getData <- function(tickers, datasrc) {
  # Wrapper for getSymbols that can handle lists of tickers. You can pass a
  # list of ticker vectors or a single ticker vector.  Recursively calls
  # getData for lists and loads xts objects into .global_env

  if (class(tickers) %in% "character") {
    for (i in 1:length(tickers)) {
      cat(tickers[i], i, "\n")
      getSymbols(tickers[i], src = datasrc,
                 auto.assign = getOption("getSymbols.auto.assign", TRUE),
                 env = globalenv())
    }
  } else {
    if (class(tickers) %in% "list") {
      for (i in 1:length(tickers)) {
        getData(tickers[[i]], datasrc)  # recursive call to getData
      }
    } else {
      return("Invalid parameter type. Should be character vector or list of
             character vectors.")
    }
  }
}
getManagerData <- function(path) {
  # This function reads in a data series and returns xts variables containing
  # useful raw data returns, and some indices see below for return data.
  # Returns a list: the raw data, returns of the managers, a wealth index
  # created by equally weighted average of the managers, returns from that
  # composite index.

  if (is.null(path)) {
    return(NULL)
  }
  mydata = read.csv(path, header = TRUE, sep = ",")

  # Clean up the data a bit...
  comps <- mydata
  dts <- as.Date(comps$Date, format = "%Y-%m-%d")  # Grab the date column...
  comps <- mydata[, 2:ncol(mydata)]  # no date column...
  comps <- xts(coredata(comps), dts)  # Now our data is in xts format...

  # Let's check to see whether the user passed return data or wealth index
  # data and do the appropriate thing...
  if (as.numeric(comps[1, 1]) == 1) {
    # We have wealth index data...
    comps <- na.locf(comps, na.rm = FALSE)
    comps <- comps * 100
    comps.ret <- xts(apply(comps, MARGIN = 2,
                           FUN = Return.calculate, method = "discrete"), dts)
    comps.ret[is.na(comps.ret)] <- 0
  } else {
    # We have return data...
    comps.ret <- comps
    comps.ret[is.na(comps.ret)] <- 0  # replace any NA's with zero retuns.
  }

  # Make an index from the peers frame...
  comps.index.ret <- as.xts(apply(comps.ret, MARGIN = 1, FUN = mean))
  comps.index <- makeIndex(comps.index.ret, inv = FALSE, ret = TRUE)

  # Now create the list that holds all the variables and return...
  return(list(mydata = mydata, comps.ret = comps.ret,
              comps.index = comps.index, comps.index.ret = comps.index.ret))
}
tickersToXTS <- function(tickers, na.omit = TRUE) {
  # take a character list of tickers and return an xts object of the
  # variables. Useful for creating objects after a call to getData().

  list.variables <- lapply(tickers, get)  # a list of actual variables
  lst.len <- length(list.variables)
  out <- as.zoo(list.variables[[1]])
  if (lst.len == 1) {
    # the single element list case...
    if (na.omit) {
      out <- na.omit(out)
    }
    return(as.xts(out))
  } else {
    # list of length > 1
    for (i in 2:length(list.variables)) {
      i <- list.variables[[i]]
      out <- merge.zoo(out, as.zoo(i))
    }
    ifelse(na.omit, out <- na.omit(out))
    return(as.xts(out))
  }
}
plotCovEllipse <- function(a, b) {
  # This function makes a scatterplot of a and b.  It plots the sample
  # histogram in the margins It overlays the kernal density for a and b.  It
  # overlays the covariance ellipse for a and b calculating ellipses

  a <- as.numeric(coredata(a))
  b <- as.numeric(coredata(b))

  if (!(length(a) == length(b))) {
    return("Variables are not of conformable length.")
  } else {
    # Calculate some eigen vector/value stuff first.

    m <- matrix(cov(a, b), 2, 2)
    diag(m) <- c(var(a), var(b))
    ev <- eigen(m)
    eval <- sqrt(ev$values)
    evec <- ev$vectors

    # Make the ellipsoid from scrath...
    u <- eval[1]
    v <- eval[2]
    x0 <- mean(a)
    y0 <- mean(b)
    alpha <- atan(evec[, 1][2] / evec[, 1][1])  #arctan of ratio of eigenvectors
    theta <- seq(0, 2 * pi, length = (1000))

    x <- x0 + u * cos(theta) * cos(alpha) - v * sin(theta) * sin(alpha)
    y <- y0 + u * cos(theta) * sin(alpha) + v * sin(theta) * cos(alpha)
    el <- data.frame(cbind(x, y))

    # df_ell <- ellipse(m, scale=c(sd(a),sd(b)), centre=c(mean(a),mean(b)))

    xend1 <- max(el$x)
    yend1 <- el[which.max(el[, 1]), 2]
    xend2 <- el[which.min(el[, 2]), 1]
    yend2 <- min(el$y)
    angle <- getAngle(c(xend1, yend1), c(xend2, yend2))

    df <- data.frame(x = a, y = b)

    hist_top <- ggplot() +
      geom_histogram(aes(a), colour = "grey60", binwidth = 0.1) +
      ggtitle("Invariant Analysis for Two Assets")
    empty <- ggplot() + geom_smooth(aes(a, b), colour = "red") +
      theme(legend.position = "none")

    hist_right <- ggplot() +
      geom_histogram(aes(b), colour = "grey60", binwidth = 0.1) +
      coord_flip()

    scatter <- ggplot(data = df, aes(x = x, y = y), colour = "red",
                      environment = environment()) +
      geom_point(size = 1.5, alpha = 0.8) + geom_path(data = el,
                                                      aes(x = x, y = y), colour = "pink", size = 1.3, linetype = 1) +
      geom_point(aes(x = mean(x), y = mean(y)), colour = "red", size = 3.5) +
      geom_segment(aes(x = mean(x), y = mean(y), xend = xend1,
                       yend = yend1), colour = "red",
                   arrow = arrow(length = unit(0.3, "cm"))) +
      geom_segment(aes(x = mean(x),
                       y = mean(y), xend = xend2, yend = yend2), colour = "blue",
                   arrow = arrow(length = unit(0.3, "cm"))) +
      ggtitle(paste("Angle: ", round(angle,
                                     3)))

    # drawing
    grid.arrange(hist_top, empty, scatter, hist_right, ncol = 2, nrow = 2,
                 widths = c(3, 1), heights = c(1, 3))
  }
}
plotFitSummary <- function(y, yhat, leverage.factor = 1,
                           net.return = NULL, ...) {
  # Takes a set of actual values, y, and a set of predicted values, yhat,
  # and produces a summary plot of the fit. Useful after running any fitting
  # routine where actual and fitted values are meaningful. Not tested on
  # categorical responses.

  # Some panel functions that drive the plot...
  monthly.return.panel <- function(...) {
    # mtext(c('Actual and Predicted Returns'), side=1, adj=1, line=-3,cex=.8)
    default.panel(...)
    # abline(h=pretty(c(par('yaxp')[1],par('yaxp')[2]),n=par('yaxp')[3]),col='gray60',lty=3)
    abline(h = 0, col = "grey30")
  }

  cum.return.panel <- function(...) {
    # mtext('Cumualtive Returns', side=1, adj=1, line=-3,cex=.8)
    default.panel(...)
    # abline(h=pretty(c(par('yaxp')[1],par('yaxp')[2]),n=par('yaxp')[3]),col='gray60',lty=3)
    abline(h = 100, col = "grey30")
  }

  diff.panel <- function(x, y, ...) {
    # type='h',lwd=5
    plus.minus.colors <- ifelse(y < 0, "red", "green4")
    lines(x, y, type = "h", col = plus.minus.colors, lwd = 5, pch = 0)
    # mtext('Actual - Predicted Monthly Returns', side=1, adj=1, line=-3,cex=.8) default.panel(x,y,...,type='h',lwd=5)
    # abline(h=pretty(c(par('yaxp')[1],par('yaxp')[2]),n=par('yaxp')[3]),col='gray60',lty=3)
    abline(h = 0, col = "black")  #par('usr')[3]
  }

  # Set up the data for plotting...
  if (!is.null(net.return)) {
    nr.start.date <- paste(first(index(y)), "/", sep = "")
    nr.index <- makeIndex(net.return[nr.start.date])
    plotData <- merge.xts(y, yhat,
                          (y - yhat),
                          makeIndex(y),
                          makeIndex(yhat),
                          nr.index,
                          makeIndex(leverage.factor * yhat))
    names(plotData) <- c("Actual", "Predicted",
                         "Monthly Difference (bps)",
                         "Cumulative Actual (Gross)",
                         "Cumulative Predicted",
                         "Cumulative Actual (Net)",
                         "Cumulative Predicted (Risk Matched)")
  } else {
    plotData <- merge.xts(y, yhat,
                          (y - yhat),
                          makeIndex(y),
                          makeIndex(yhat),
                          makeIndex(leverage.factor * yhat))
    names(plotData) <- c("Actual", "Predicted",
                         "Monthly Difference (bps)",
                         "Cumulative Actual (Gross)",
                         "Cumulative Predicted",
                         "Cumulative Predicted (Risk Matched)")
  }

  ## Make the plot...
  plot.xts(plotData, screens = c(1, 1, 2, 3, 3, 3, 3),
           layout.screens = matrix(c(1, 1, 2, 3, 3, 3, 3), 2, 2, byrow = T),
           lwd = c(2, 2, 1, 2, 2, 2,2),
           las = 1, lty = c(1, 1, 1, 1, 1, 1, 1),
           col = c("grey60", "red", "green", "dark grey", "red", "blue"),
           yax.loc = "left",
           ylab = NA,
           minor.ticks = F,
           auto.legend = T,
           legend.pars = list(bty = "n", horiz = F),
           panel = c(monthly.return.panel,
                     diff.panel, cum.return.panel),
           main = NA,
           bty = "n")
  title(main = "Performance Summary of Factor Model - Out-of-Sample",
        outer = T, line = -1.5, mgp = c(4, 1, 0))
}
getAngle <- function(u, v) {
  # given two vectors u an v, return the angle between them.

  numerator <- u[1] * v[1] + u[2] * v[2]
  denominator <- sqrt(u[1]^2 + u[2]^2) * sqrt(v[1]^2 + v[2]^2)

  out <- (acos(numerator / denominator))/0.0174532925
  return(out)
}
volAdjust <- function(a, target.vol = 1) {
  # takes an xts object x and scales it by target.vol
  if (ncol(a) == 1) {
    vol.scale.factor <- target.vol / sd(a)
    out <- a * vol.scale.factor
  } else {
    vol.scale.factors = xtsApply(a, function(x) target.vol / sd(x))
    out <- a * vol.scale.factors
  }
  return(out)
}
repRow <- function(x, n) {
  matrix(rep(x, each = n), nrow = n)
}
repCol <- function(x, n) {
  matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}
simpSignal <- function(data, lookback = 12) {
  # This function takes a vector of price signals and returns a vector
  # of momentum signals based on the supplied lookback width. lookback
  # default is 12 months. data is assumed to be monthly.

  f <- xts::periodicity(data)$scale
  if (f != "monthly") {
    return("This function only accomodates monthly data at present.")
  } else {
    d <- na.omit(data)
    far.prices <- lag(d, lookback)
    near.prices <- lag(d, 1)
    signals <- sign(near.prices - far.prices)
    return(signals)
  }
}
wtdAvgVol <- function(w, R, equal.wt = TRUE, scale = 1) {
  # Given a weight vector and an xts object of returns, this will return the
  # weighted average volatility assuming all the assets are perfectly
  # uncorrelated.  (ie. all pairwise covariances are zero) If equal.wt=TRUE,
  # then w is ignored. Otherwise, w is used for the weight vector.

  if (equal.wt == TRUE) {
    w <- rep(1/ncol(R), ncol(R))
  }
  avg.vol <- apply(R, 2, sd) * sqrt(scale)
  wtd.avg.vol <- t(w) %*% avg.vol

  return(wtd.avg.vol)
}
portfolioVol <- function(w, R, sigma, equal.wt = T, scale = 252) {
  # Compute the portfolio vol from returns, weights, and the covariance matrix
  # supplied by the user.

  if (equal.wt == T) {
    w <- rep(1/ncol(R), ncol(R))
  }
  S <- sigma
  out <- sqrt(t(w) %*% S %*% w) * sqrt(scale)
  return(out)
}
divRatio <- function(w, R, sigma, equal.wt = TRUE, scale = 12) {
  # computes the Diversification Ratio from weights, returns, and a covariance
  # matrix.

  wav <- wtdAvgVol(w = w, R = R, equal.wt = equal.wt, scale = scale)
  pvol <- portfolioVol(w = w, R = R, sigma = sigma,
                       equal.wt = equal.wt, scale = scale)
  dr <- wav / pvol
  colnames(dr) <- "Diversification Ratio"
  return(dr)
}
scrubNames <- function(x) {
  # takes an xts object containing fund names and replaces the naems with
  # Fund.x where x is 1 to ncol(x)

  out <- x
  names.scrubbed <- paste("Fund.", seq(1, ncol(x), 1), sep = "")
  colnames(out) <- names.scrubbed
  return(out)
}
makeGrossReturns <- function(nr, mgmt.fee = 0.02, perf.fee = 0.2,
                             highwater = TRUE, ann.exp = 0.0025,
                             scale = 12) {
  # This function takes a net return stream (nr) and approximates a gross
  # return stream given the supplied parameters. nr should be in xts format.

  if (!is.xts(nr))
    stop("Must supply function with xts object")
  dts <- index(nr)
  nr.index <- makeIndex(nr)
  gr <- NULL
  gr[1] <- nr[1] + (mgmt.fee + ann.exp)/scale

  for (i in 2:nrow(nr)) {
    ## Step 1: gross up nr by the de-annualized fixed expenses
    gr[i] <- nr[i] + (mgmt.fee + ann.exp)/scale

    # Now, we need to check to see if we are above or below the high
    # water mark...
    hwm <- max(nr.index[0:(i - 1)])
    above.hwm <- ifelse(hwm < nr.index[i], TRUE, FALSE)

    if (above.hwm) {
      ## manager earns performance fee, so we need to gross up the net return...
      gr[i] = gr[i] * (1 + perf.fee)
    }
  }
  gr <- xts(gr, dts)
  names(gr) <- "Gross Return"
  return(gr)
}
predictionError <- function(y, yhat) {
  # Given two series, the actual values y and the predicted values yhat,
  # returns the prediction error given by sqrt((mean(y-yhat))^2)

  ydiff <- y - yhat

  if (ncol(y) > 0) {
    pe <- apply(ydiff, 2, function(x) sqrt(mean((x)^2)))
  } else {
    # we have a single column vector...
    pe <- sqrt(mean((y - yhat)^2))
  }
  names(pe) <- names(y)
  return(pe)
}
hitRatio <- function(x, y = NULL) {
  # Given a single track record (x), will return the percentage of observations
  # greater than or equal to zero.  Given two track records (x and y),
  # will compute the difference (x-y)and return the percentage of time the
  # difference was positive.

  if (is.null(y)) {
    # only one track record supplied...
    out <- sum(as.numeric(x > 0))/length(x)
  } else {
    # two track records supplied...
    if (length(x) != length(y)) {
      return("x and y lengths differ. Recycling not appropriate.")
    } else {
      diff.xy <- x - y
      out <- sum(as.numeric(diff.xy > 0))/length(diff.xy)
    }
  }
  return(out)
}
tuneCorrelationCap <- function(train.data, test.data, cor.start = 0.3,
                               cor.incr = 0.01) {

  # This function tunes the correlation cap on the factors by examining the
  # minimum prediction error of the resltant models.  This attempts to
  # optimize the tradeoff of being able to model the asset more effectively and
  # having large collinearity issues in the coefficients.  Requires caret
  # annd fitTsfm.

  # Parameters to be passed to fitTsfm.
  mkt.name <- NULL
  rf.name <- NULL
  fit.method <- "OLS"
  variable.selection = "subsets"
  mkt.timing <- NULL
  really.big <- F  # Set to true if # factors greater than 50...

  factors <- train.data[, 2:ncol(train.data)]
  asset <- train.data[, 1]
  asset.names <- names(asset)
  cor.factors <- cor(factors)
  out.err <- NULL
  data <- NULL

  nsteps <- (1 - cor.start)/cor.incr
  cat(nsteps, "\n")
  for (i in 0:nsteps) {
    high.cor.factors <- findCorrelation(cor.factors,
                                        cutoff = (cor.start + cor.incr * i))
    reduced.factors <- factors[, -high.cor.factors]

    if (ncol(reduced.factors) == 0) {
      out.err[i] <- NA
    } else {
      factor.names <- colnames(reduced.factors)
      data <- merge.xts(assets, reduced.factors, join = "inner")

      fit <- fitTsfm(asset.names = asset.names, factor.names = factor.names,
                     rf.name = rf.name, data = data, fit.method = fit.method,
                     variable.selection = variable.selection,
                     mkt.timing = mkt.timing, really.big = really.big)

      predicted.vals <- predict(fit, newdata = test.data)
      actual.vals.test <- test.data[, 1:length(predicted.vals)]
      pred.v <- xts(matrix(unlist(predicted.vals), ncol = length(predicted.vals)
                           , byrow = F),
                    index(na.omit(actual.vals.test)))

      colnames(pred.v) <- asset.names
      out.err[i] <- c(predictionError(actual.vals.test, pred.v))
      cat(paste(i, " ", cor.start + cor.incr * i, " ", out.err[i]), "\n")
    }
  }
  return(out.err)
}
