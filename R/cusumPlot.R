
#' cusumPlot
#'
#' displays a straight-forward CUSUM plot
#'
#' @param O vector of observed rate/probability
#' @param E vector of expected rate /probability
#' @param sort.var variable to sort to CUSUM
#' @param ..., additional parameters passed to base plot & lines functions
#' @keywords cusum
#' @export
#' @examples
#' O <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
#' E <- c(0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.1, 0.1,
#'        0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4)
#' sort.var <- 1:20
#' cusumPlot(O, E, sort.var, xlab = "case #", ylab = "avoided cases", main = "Title", col = "dark blue")

cusumPlot <- function(O, E, sort.var, ...)

{
    eVar <- E*(1-E) # calculate the eVar
    sort.ind <- order(sort.var) # sort the O,E, eVar and sort.var by sort.var
    sort.var <- sort.var[sort.ind]
    O <- O[sort.ind]
    E <- E[sort.ind]
    eVar <- eVar[sort.ind]

    cumsum.O <- cumsum(O)
    cumsum.E <- cumsum(E)
    cumsum.eVar <- cumsum(eVar)

    diff <- c(0, cumsum.E-cumsum.O)
    ciL <- c(0,-1.96*sqrt(cumsum.eVar))
    ciH <- c(0,1.96*sqrt(cumsum.eVar))
    sort.var <- c(sort.var[1],sort.var)

    plot(sort.var, diff, type="s", lwd=3, ylim=range(c(diff,ciL,ciH)), ...)
    abline(h=0, lty=2, col="black")
    lines(sort.var, ciH, lwd=2, type="s", ...)
    lines(sort.var, ciL, lwd=2, type="s", ...)

}### end of cusumPlot() function
