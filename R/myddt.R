#' @title myddt (Project1)
#'
#' @param df data frame
#' @param sp species
#'
#' @return A sophisticated plot, along with console information
#' @export
#'
#' @import ggplot2 dplyr utils
#'
#' @examples
#' \dontrun{myddt(ddt, "CCATFISH")}
myddt <- function(df, sp) {
  SPECIES <- RIVER <-  WEIGHT <- LENGTH <- NULL
  df1 <- df %>% filter(SPECIES == {{sp}})
  g <- ggplot(df1, aes_string(x="LENGTH", y="WEIGHT")) +
    geom_point(aes_string(color = "RIVER"), size = 2) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") +
    ggtitle("Michael Jarriel")
  print(g)
  write.csv(df1, paste(getwd(), "/LvsWfor", sp, ".csv", sep=""), row.names = FALSE)
  mylist <- list(Before = df, After = df1, Table = (table(df$RIVER)/length(df$RIVER)))
  mylist
}
