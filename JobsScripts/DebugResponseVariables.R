load(file = "Positions_ts2014-05-08_2019-05-07.Rdata")
Positions_ts %<>% lapply(wind = wind, verbose = T, function(l, wind, verbose){
  dat <- cbind(quantmod::OHLC(l),l[,"sar_i"])
  cl_nm <- grep("close", names(dat), ignore.case = T)
  sa_nm <- grep("sar", names(dat), ignore.case = T)
  if (all(!is.na(dat))) {
    rvs <- purrr::map(wind, env = parent.frame(), function(.x, env){
      if (nrow(dat) <= .x) .x <- {nrow(dat) - 1}
      ras <- zoo::rollapplyr(dat[,c(cl_nm, sa_nm)], width = .x, align = "left", by.column = F, env = parent.frame(), na.pad = T, partial = T, function(r, env){
        if (!is.na(which(r[, 2] == -1)[1])) s <- which(r[, 2] == -1)[1] else s <- numeric(0)
        #if(verbose) print(c(s = s,sd = sd(r[,1])))
        p <- tryCatch({quantmod::findPeaks(r[, 1], thresh = stats::sd(r[,1]))}, error = function(e) numeric(0))
        v <- tryCatch({quantmod::findValleys(r[, 1], thresh = stats::sd(r[,1]))}, error = function(e) numeric(0))
        if (length(s) != 0) i <- s[1] else if (length(p) != 0) i <- p[1] else if (length(v) != 0) i <- v[1] else i <- .x
        #if(verbose) print(i)
        out <- {zoo::coredata(r[i, 1]) - zoo::coredata(r[1, 1])}
        #if(verbose) print(out)
        return(out)
      })
      # Fill the NA at the end
      na_ind <- which(is.na(ras)) 
      n <- min(na_ind)
      end <- max(na_ind)
      
      while (n < end) {
        if (verbose) print(n:end)
        # Use the SAR as the Sell point first, Which returns index within n:na_ind, so to get the actual index it needs to be used as a subset of n:end
        if (!is.na(which(dat[n:end, sa_nm] == -1)[1])) s <- c(n:end)[which(dat[n:end, sa_nm] == -1)[1]] else s <- numeric(0)
        p <- tryCatch({c(n:end)[quantmod::findPeaks(dat[n:end, cl_nm], thresh = stats::sd(dat[n:end, cl_nm]))]}, error = function(e) numeric(0))
        v <- tryCatch({c(n:end)[quantmod::findValleys(dat[n:end, cl_nm], thresh = stats::sd(dat[n:end, cl_nm]))]}, error = function(e) numeric(0))
        if(verbose) print(c(s=s,p=p,v=v))
        if (length(s) != 0) i <- s[1] else if (length(p) != 0) i <- p[1] else if (length(v) != 0) i <- v[1] else i <- end
        if(verbose == T) print(i)
        ras[n] <- {zoo::coredata(dat[i, cl_nm]) - zoo::coredata(dat[n, cl_nm])}
        n <- n + 1
      }
      ras[n] <- 0
      # END
      return(ras)
    })
    out <- do.call("cbind", rvs)
    colnames(out) <- paste0(names(out),"_rv")
    l <- l[,- grep("rv$",names(l))]
    out <- cbind(l, out)
  }else out <- l
  return(out)
})
