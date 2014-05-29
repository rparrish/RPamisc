#' dotplot.errors
#'
#' queries EDW using windows credentials
#' @param title The title of the measure
#' @param name vector of the names of each observation (ie. hospital, provider)
#' @param measure vector of measure values
#' @param upper vector with each upper confidence interval
#' @param lower vector with each lower confidence interval
#' @param benchmark the average or benchmark value
#' @keywords dotplot
#' @return plots a simple dotplot with title and benchmark as a horizontal dashed line.
#' @export
#' @examples
#' group <- c("PAMC", "PHCMC", "PLCMT", "PMMC", "PPMC", "PRMCE", "PSHMC", "PSJMC", "PSPH", "PSVMC", "PTMC", "SMCCH", "SPHHSC")
#' measure <- c("7.370184255", "3.658536585", "0.45045045", "9.139784946", "5.813953488", "2.23325062", "1.752848379", "4.909560724", "2.777777778", "3.610108303", "0", "1.50862069", "6.425041186")
#' lower <- c("5.405954443", "2.06185199", "0.054598268", "5.414712287", "3.58711388", "1.328809357", "1.073889102", "2.981406318", "1.768844576", "2.448730538", "0", "0.827166709", "4.608364085")
#' upper <-  c("9.768064814", "5.962475146", "1.61763621", "14.23027598", "8.836604532", "3.506601868", "2.694203006", "7.560926463", "4.139000759", "5.113740293", "1.30881647", "2.518265819", "8.678874819")
#' benchmark <- "3.350879445"
#'
#' mydata <- data.frame(group=group, lower=lower, est=measure, upper=upper)
#'
#' dotplot.errors(x, reference.line=benchmark)
#'

dotplot.errors <- function(x, myTheme = simpleTheme(pch = 19, col = 1),
                           qlabel = "Estimated rate", add.text.to.qlabel = "", type.bar = "CI",
                           conf.level = .95, end.length = .05, reorder.groups = TRUE,
                           reordering = "decreasing", label.define = list(), reference.line = NULL,
                           bar.color = 1, horizontal = TRUE, ...)
{ require(lattice)
  o <- c(which(colnames(x) == "group"), which(colnames(x) == "lower"),
         which(colnames(x) == "est"), which(colnames(x) == "upper"))
  if (length(o) != 4) stop("Error: Incorrect data frame")
  x <- x[, o]
  x$group <- factor(x$group)
  if (horizontal == T) hor <- 1 else hor <- -1
  if (reordering == "decreasing")
      FUN.to.reorder <- function(x) mean(x) * hor
  else FUN.to.reorder <- function(x) -mean(x) * hor
  if (reorder.groups) x$group <-
      with(x, reorder(group, est, FUN = FUN.to.reorder))

  if (reorder.groups) {
      if (reordering == "increasing") {
          x$group = factor(x$group, levels=x$group[order(-x$est, -x$lower, -x$upper, -x$group)], ordered=TRUE)
      } else {
          x$group = factor(x$group, levels=x$group[order(x$est, x$lower, x$upper, x$group)], ordered=TRUE)
      }
  }

  if (type.bar == "CI") xlab <- substitute(expression(lab %+-% CL),
                                           list(lab = qlabel, CL = paste(" ", as.character(conf.level * 100),
                                                                         "% CI", add.text.to.qlabel, sep = ""))) else
                                                                             if (type.bar == "SE")
                                                                                 xlab <- substitute(expression(lab %+-% " SE" ~ ~ AT),
                                                                                                    list(lab = qlabel, AT = add.text.to.qlabel)) else
                                                                                                        xlab <- substitute(expression(lab %+-% Type.bar ~ ~ AT),
                                                                                                                           list(lab = qlabel, Type.bar = type.bar, AT = add.text.to.qlabel))
  if (horizontal == T)
      p <- stripplot(group ~ est, data = x,
                     lower = x$lower, upper = x$upper,
                     xlim = range(x[,2:4])+c(-.04,.04)*diff(range(x[,2:4])),
                     xlab = c(list(xlab), label.define),
                     par.settings = myTheme,
                     panel = function(x, y, lower, upper, ..., subscripts) {
                         if (is.null(reference.line) == F)
                             panel.abline(v = reference.line, col = "grey")
                         panel.abline(h = y, col = "grey", lty = "dashed")
                         panel.text(reference.line + .5, 1, labels=round(reference.line,2))
                         panel.arrows(x0 = lower[subscripts], y0 = y,
                                      x1 = upper[subscripts], y1 = y, angle = 90, code = 3,
                                      length = end.length, col = bar.color)
                         panel.stripplot(x, y, ...) }, ...) else
                             p <- stripplot(est ~ group, data = x, lower = x$lower, upper = x$upper,
                                            ylim = range(x[,2:4])+c(-.04,.04)*diff(range(x[,2:4])),
                                            ylab = c(list(xlab), label.define),
                                            par.settings = myTheme,
                                            panel = function(x, y, lower, upper, ..., subscripts) {
                                                if (is.null(reference.line) == F)
                                                    panel.abline(h = reference.line, col = "grey")
                                                panel.abline(v = x, col = "grey", lty = "dashed")
                                                panel.text(v=x, y=.55, labels=reference.line)
                                                panel.arrows(y0 = lower[subscripts], x0 = x,
                                                             y1 = upper[subscripts], x1 = x, angle = 90, code = 3,
                                                             length = end.length, col = bar.color)
                                                panel.stripplot(x, y, ...) }, ...)
  print(p)
  invisible(p)
}



