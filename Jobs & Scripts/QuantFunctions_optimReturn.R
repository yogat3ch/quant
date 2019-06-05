#' Optimize returns
#' 
#' Allows for optimization of returns from an xts, matrix, data.frame or tibbletime object given the column with returns threshold as percent and the target percentage of returns expected
#' @param dat \code{[xts, data.frame, tibble_time]} Data to be used **Required**
#' @param percent \code{[numeric]} Percent value as proportion to used as a threshold for buy. Defaults to 0.05 **Required**
#' @param returns.clm \code{[character]} The name or number of the column with returns (in percents) **Required**
#' @param tslindex.clm \code{[character]} The name or number of the column with the index of days following the setting of a trailing stop loss before the sale executes. **Required**
#' @param cumulative \code{[Boolean]} \code{FALSE} indicating  returns should be summed serially  (assuming a fixed amount is invested at each buy point) or if returns should be summed cumulatively \code{TRUE} (all gains are reinvested in subsequent buys). Defaults to \code{TRUE}
#' @return \itemize \code{[numeric]} A vector indicating buy points as 1, sell points as -1, and all other values 0 with the following attributes:
#' \itemize{
#'  \item{\code{Returns}}{A percent indicating the total percent of returns expected }
#'  \item{\code{Actions}}{ The number of actions (if commission fees are charged, useful for calculating total charges) }
#' }
optimReturn <- function(dat, percent = 0.05, returns.clm = NULL, tslindex.clm = NULL) {
  tslindex.clm <- ifelse(!is.null(tslindex.clm),tslindex.clm,paste0(returns.clm,"_ind"))
  # message(paste(returns.clm, tslindex.clm))
  v <- dat[, returns.clm, drop = T]
  v.ind <- dat[, tslindex.clm, drop = T]
  i <- 1
  v_rows <- ifelse(!is.null(nrow(v)), nrow(v), length(v))
  # message(paste(v %>% class))
  # message(paste(v.ind %>% class))
  
  # Loop through returns
  # if (xts::is.xts(dat)) time_index <- time(dat) else time_index <- tibbletime::get_index_col(dat)
  out <- rep.int(0, v_rows)
  init.inv <- c()
  cum.returns <- 0
  total.returns <- 0
  while (i <= v_rows) {
    ind <- 1
    # If this is a buy point
    
    if (v[i] >= percent) {
      if (is.null(init.inv)) { # Start with initial parameters
        init.inv <- dat[i,"close", drop = T] %>% as.numeric #The running investment & initial
        shares <- 1 # The number of shares
      }
      out[i] <- 1 # Indicate so on the out vector
      if (!is.null(tslindex.clm)) {
        ind <- as.numeric(v.ind[i]) # Get the # of days until sell
        out[i + ind] <- -1 # Indicate it as a vector
        if ({cum.returns %/% as.numeric(dat[i,"close", drop = T])} > shares) {
          # If the cum.returns allows the purpose of more shares
          shares <- {cum.returns %/% as.numeric(dat[i,"close", drop = T])} # Update shares
        }
       
        cum.returns <- as.numeric(dat[i + ind,"close", drop = T]) * shares - as.numeric(dat[i,"close", drop = T]) * shares + cum.returns
        
        total.returns <- as.numeric(dat[i + ind,"close", drop = T]) - as.numeric(dat[i,"close", drop = T]) + total.returns
        ind <- ind + 1 # Add 1 where the loop will start back
      }
      
      
    }else out[i] <- 0
    i <- i + ind
  }
  out <- data.frame(factor(out))
  attr(out, "Actions") <- sum(out != 0)
  attr(out, "Returns") <- (total.returns - init.inv) / init.inv
  attr(out, "Cum.Returns") <- (cum.returns - init.inv) / init.inv
  colnames(out) <- paste0(returns.clm,"_ret")
  return(out)
}