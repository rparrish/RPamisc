
#' cusumPlot
#'
#' displays a straight-forward CUSUM plot
#'
#' @param .data dataframe containing the Observed, Expected and grouping variable, if any
#' @param O name of column with observed rate/probability
#' @param E name of column with expected rate /probability
#' @param group column with grouping variable used to facet plots
#' @param ylab y-axis label. default is "Cases Avoided"
#' @param title plot title. default is blank
#' @keywords cusum
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by mutate row_number
#' @import ggplot2
#' @import lattice
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' oe_data <- data.frame(
#'      O = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'            0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
#'      E = c(0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.1, 0.1,
#'            0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4),
#'      group = c(rep("Apple", 10), rep("Cherry", 10))
#'      )
#'
#' cusumPlot(oe_data, "O", "E", "group", ylab = "avoided cases", title = "Title")

cusumPlot <- function(.data,
                      O = "Obs",
                      E = "Exp",
                      group = NA,
                      ylab = "Cases Avoided", 
                      title = "")  {
    
    # takes care of 'no visible binding for global variable warnings
    eVar <- cumsum_E <- cumsum_O <- cumsum_eVar <- ciH <- ciL <- index <- NULL

    if (!is.na(group)) {
        plot_data <-
            data.frame(O = .data[[O]],
                       E = .data[[E]],
                       group = .data[[group]]) %>%
            group_by(group)

    } else {
        plot_data <-
            data.frame(O = .data[[O]], E = .data[[E]])
    }

    plot_data <-
        plot_data %>%
        mutate(index = row_number(),
               eVar = E * (1 - E),
               cumsum_O = cumsum(O),
               cumsum_E = cumsum(E),
               cumsum_eVar = cumsum(eVar),
               diff = cumsum_E - cumsum_O,
               ciL = -1.96 * sqrt(cumsum_eVar),
               ciH = 1.96 * sqrt(cumsum_eVar)
        )

    p <- ggplot(data = plot_data) +
         aes(x = index, y = diff) +
         geom_step() +
         geom_step(aes(y = ciH)) +
         geom_step(aes(y = ciL)) +
         geom_hline(aes(yintercept = 0), linetype = 2) +
         xlab("") +
         ylab(ylab) +
         ggtitle(title) +
         theme_bw() +
         theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank())

    if("group" %in% names(plot_data)) p <- p + facet_wrap(  ~ group)
    p

}
