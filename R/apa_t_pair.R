#' Round your number while retaining trailing zeroes
#'
#' @param x Vector of numbers to round.
#' @param digits Number of digits to round your number to.
#'
#' @return A character string for your rounded number.
round0 <- function(x, digits = 0) {
  fmt <- paste0("%.", digits, "f")
  x0 <- sprintf(fmt, x)

  return(x0)
}

#' APA text for Paired-Samples T-Test
#'
#' @description
#' Create APA-formatted text for the results of an independent-samples t-test in the following format:
#'
#' A paired-samples t-test was conducted to compare \{dv\} between \{level1\} (M = \{mean1\}, SD = \{sd1\}) and \{level2\} (M = \{mean2\}, SD = \{sd2\}). There was a \{non\}significant difference; t(\{df\}) = \{t_value\}, p = \{p_value\}.
#'
#' @param xcol A vector of the values for x column.
#' @param ycol A vector of the values for y column.
#' @param dv The text describing the DV in the output statement.
#' @param level1 The text describing level 1 in the output statement.
#' @param level2 The text describing level 2 in the output statement.
#' @param alpha Value for critical alpha to compare the p-value against.
#'
#' @return A character string.
#' @export
#'
#' @examples
#' # use generic text
#' apa_t_pair(x = self_res_att$f_self,
#'            y = self_res_att$f_non)
#'
#' # specify the text for dv and levels
#' apa_t_pair(xcol = self_res_att$f_self,
#'            ycol = self_res_att$f_non,
#'            dv = "preferences for female faces",
#'            level1 = "participants who resembled those faces",
#'            level2 = "non-self participants")
apa_t_pair <- function(xcol,
                       ycol,
                       dv = "dv",
                       level1 = "level 1",
                       level2 = "level 2",
                       alpha = .05){
  # analysis
  t_results <- stats::t.test(
    x = xcol,
    y = ycol,
    paired = TRUE)

 # template
  template <- "A paired-samples t-test was conducted to compare {dv} between {level1} (M = {mean1}, SD = {sd1}) and {level2} (M = {mean2}, SD = {sd2}). There was a {non}significant difference; t({df}) = {t_value}, p {p_value}."

  glue::glue(
    template,
    dv      = dv,
    level1  = level1,
    level2  = level2,
    mean1   = round0(mean(xcol), 1),
    sd1     = round0(stats::sd(xcol), 1),
    mean2   = round0(mean(ycol), 1),
    sd2     = round0(stats::sd(ycol), 1),
    non     = ifelse(t_results$p.value < alpha, "", "non-"),
    df      = round0(t_results$parameter, 0),
    t_value = round0(t_results$statistic, 2),
    p_value = ifelse(t_results$p.value < .001,
                     "< .001",
                     paste0("= ",  round0(t_results$p.value, 3)))
  )
}

