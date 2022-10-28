#' Control Zeros on Rounding Digits
#'
#' @param x A (non-empty) numeric vector of data values.
#' @param digits Integer indicating the number of decimal places (round).
#'
#' @description
#' round0 rounds the values in its first argument to the specified number of decimal places. In contrast to round, this function retains trailing zeroes to comply with APA style.
#'
#' @return A character string providing the rounded value with specified number of decimal places.
#' @export
#'
#' @examples
#'# Using only the number argument with default decimals
#'round0(x = 1.83028)
#'
#'# Controlling the number of decimals to round to
#'round0(x = 1.83028,
#'       digits = 3)

round0 <- function(x, digits = 0){
  # Create formatting string
  fmt <- paste0("%.", digits, "f")
  x0 <- sprintf(fmt, x)

  return(x0)
}

#' APA Text for Paired Samples T-Test
#'
#' @description
#' Create APA formatted text for the results of a paired samples t-test in the following format:
#'
#' A paired-samples t-test was conducted to compare \{dv\} between \{level1\} (M = \{mean1\}, SD = \{sd1\}) and \{level2\} (M = \{mean2\}, SD = \{sd2\}). Using an alpha of \{alpha\}, there was a \{non\}significant difference; t(\{df\}) = \{t_value\}, \{p_value\}, mean difference = \{mean_diff\}, \{CI\}% CI = \[\{lower_CI\}, \{upper_CI\}\].
#'
#'
#' @param x A (non-empty) numeric vector of data values for level 1.
#' @param y A (non-empty) numeric vector of data values for level 2.
#' @param dv A character string describing the DV in the output text.
#' @param level1 A character string describing level 1 in the output text.
#' @param level2 A character string describing level 2 in the output text.
#' @param alpha The critical alpha for determining statistical significance.
#' @param conf.level The confidence level for specifying the coverage of confidence intervals.
#'
#' @return A character string combining an APA style t-test with a description of the results.
#' @export
#'
#' @examples
#' # Use generic text
#' apa_t_pair(x = self_res_att$f_self,
#'            y = self_res_att$f_non)

# Specify the text for dv and level

#' apa_t_pair(x = self_res_att$f_self,
#'            y = self_res_att$f_non,
#'            dv = "preferences for female faces",
#'            level1 = "participants who resembled those faces",
#'            level2 = "non-self participants")

apa_t_pair <- function(x,
                       y,
                       dv = "the DV",
                       level1 = "level 1",
                       level2 = "level 2",
                       alpha = .05,
                       conf.level = 0.95){

  t_results <- t.test(
    x = x,
    y = y,
    paired = TRUE,
    alpha = alpha,
    conf.level = conf.level
  )

  template <- "A paired-samples t-test was conducted to compare {dv} between {level1} (M = {mean1}, SD = {sd1}) and {level2} (M = {mean2}, SD = {sd2}). Using an alpha of {alpha}, there was a {non}significant difference; t({df}) = {t_value}, {p_value}, mean difference = {mean_diff}, {CI}% CI = [{lower_CI}, {upper_CI}]."

  apa_text <- glue::glue(
    template,
    # Define descriptives
    mean1   = round0(mean(x), 1),
    sd1     = round0(sd(x), 1),
    mean2   = round0(mean(y), 1),
    sd2     = round0(sd(y), 1),
    # Check whether p-value was smaller than user defined alpha
    non     = ifelse(t_results$p.value < alpha, "", "non-"),
    # Define t-test results
    df      = round0(t_results$parameter, 0),
    t_value = round0(t_results$statistic, 2),
    # Round p-value, then format to either exact or p<.001
    p_value = round0(t_results$p.value, 3),
    p_value = ifelse(p_value < 0.001, "p < .001", paste0("p = ", p_value)),
    # Define effect sizes
    mean_diff = round0(t_results$estimate, 1),
    CI = round0(conf.level*100, 0),
    lower_CI = round0(t_results$conf.int[1], 1),
    upper_CI = round0(t_results$conf.int[2], 1)
  )

  return(apa_text)

}
