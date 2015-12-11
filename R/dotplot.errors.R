#' dotplot.errors
#'
#' build a vertical dotplot with error bars
#'
#' @param x dataframe
#' @param myTheme default values for pch and col (todo)
#' @param qlabel text for axis label
#' @param add.text.to.qlabel additional text for axis label
#' @param type.bar default is "CI"
#' @param conf.level default is .95
#' @param end.length default is .05
#' @param reorder.groups default is TRUE
#' @param reordering order of sorting when reorder.groups is TRUE
#' @param label.define (todo)
#' @param reference.line value for the reference line. default is null
#' @param bar.color default is 1
#' @param horizontal layout of the dotplot. default is TRUE
#' @param ... further arguments passed to or from other methods.
#' @author Rollie Parrish
#' @export


dotplot.errors <- function (x, myTheme = simpleTheme(pch = 19, col = 1), qlabel = "Estimated rate",
          add.text.to.qlabel = "", type.bar = "CI", conf.level = 0.95,
          end.length = 0.05, reorder.groups = TRUE, reordering = "decreasing",
          label.define = list(), reference.line = NULL, bar.color = 1,
          horizontal = TRUE, ...)
{
    o <- c(which(colnames(x) == "group"), which(colnames(x) ==
                                                    "lower"), which(colnames(x) == "est"), which(colnames(x) ==
                                                                                                     "upper"))
    if (length(o) != 4)
        stop("Error: Incorrect data frame")
    x <- x[, o]
    x$group <- factor(x$group)
    if (horizontal == T)
        hor <- 1
    else hor <- -1
    if (reordering == "decreasing")
        FUN.to.reorder <- function(x) mean(x) * hor
    else FUN.to.reorder <- function(x) -mean(x) * hor
    if (reorder.groups)
        x$group <- with(x, reorder(group, est, FUN = FUN.to.reorder))
    if (reorder.groups) {
        if (reordering == "increasing") {
            x$group = factor(x$group, levels = x$group[order(-x$est,
                                                             -x$lower, -x$upper, -x$group)], ordered = TRUE)
        }
        else {
            x$group = factor(x$group, levels = x$group[order(x$est,
                                                             x$lower, x$upper, x$group)], ordered = TRUE)
        }
    }
    if (type.bar == "CI")
        xlab <- substitute(lab %+-% CL, list(lab = qlabel, CL = paste(" ",
                                                                      as.character(conf.level * 100), "% CI", add.text.to.qlabel,
                                                                      sep = "")))
    else if (type.bar == "SE")
        xlab <- substitute(lab %+-% " SE" ~ ~AT, list(lab = qlabel,
                                                      AT = add.text.to.qlabel))
    else xlab <- substitute(lab %+-% Type.bar ~ ~AT, list(lab = qlabel,
                                                          Type.bar = type.bar, AT = add.text.to.qlabel))
    if (horizontal == T)
        p <- stripplot(group ~ est, data = x, lower = x$lower,
                       upper = x$upper, xlim = range(x[, 2:4]) + c(-0.04,
                                                                   0.04) * diff(range(x[, 2:4])), xlab = c(list(xlab),
                                                                                                           label.define), par.settings = myTheme, panel = function(x,
                                                                                                                                                                   y, lower, upper, ..., subscripts) {
                                                                                                               if (is.null(reference.line) == F)
                                                                                                                   panel.abline(v = reference.line, col = "grey")
                                                                                                               panel.abline(h = y, col = "grey", lty = "dashed")
                                                                                                               panel.arrows(x0 = lower[subscripts], y0 = y,
                                                                                                                            x1 = upper[subscripts], y1 = y, angle = 90,
                                                                                                                            code = 3, length = end.length, col = bar.color)
                                                                                                               panel.stripplot(x, y, ...)
                                                                                                           }, ...)
    else p <- stripplot(est ~ group, data = x, lower = x$lower,
                        upper = x$upper, ylim = range(x[, 2:4]) + c(-0.04, 0.04) *
                            diff(range(x[, 2:4])), ylab = c(list(xlab), label.define),
                        par.settings = myTheme, panel = function(x, y, lower,
                                                                 upper, ..., subscripts) {
                            if (is.null(reference.line) == F)
                                panel.abline(h = reference.line, col = "grey")
                            panel.abline(v = x, col = "grey", lty = "dashed")
                            panel.arrows(y0 = lower[subscripts], x0 = x, y1 = upper[subscripts],
                                         x1 = x, angle = 90, code = 3, length = end.length,
                                         col = bar.color)
                            panel.stripplot(x, y, ...)
                        }, ...)
    return(p)
}
