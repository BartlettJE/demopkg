round0 <- function(x, digits = 0){
  # Create formatting string
  fmt <- paste0("%.", digits, "f")
  x0 <- sprintf(fmt, x)

  return(x0)
}

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
