## Tests using wagepan
library(didmultipleGT)
library(tidyverse)
##### wagepan tests #####
wagepan_df <- didmultipleGT::wagepan

## Cleaning union memebership as described on page 23
# TODO: Remove tidyverse dependency
clean_wagepan_df <- wagepan_df %>%
  group_by(nr) %>%
  arrange(year) %>%
  mutate(union_clean = ifelse(lag(union) == 0 & union == 1 & lead(union) == 0,
                              0,
                              union),
         union_clean = ifelse(is.na(union_clean),
                              union,
                              union_clean),
         union_clean = ifelse(lag(union) == 1 & union == 0 & lead(union) == 1,
                              1,
                              union_clean),
         union_clean = ifelse(is.na(union_clean),
                              union,
                              union_clean))




## Two Way FE
two_way_fe_model <- lm(data = clean_wagepan_df,
                 lwage ~ union_clean + factor(nr) + factor(year))

fe_coef <- two_way_fe$coefficients["union_clean"]

# First Differenced

fd_clean_wagepan_df <- clean_wagepan_df %>%
  mutate(lwage_d = lwage - lag(lwage),
         union_clean_d = union_clean - lag(union_clean))


fd_model <- lm(data = fd_clean_wagepan_df,
               lwage_d ~ union_clean_d)

fd_coef <- fd_model$coefficients["union_clean_d"]

## No controls
no_control_did_gt <- did_multiplegt(data = wagepan_df,
                                    variable_list = c("lwage",
                                                      "nr",
                                                      "year",
                                                      "union"),
                                    placebo = 2,
                                    breps = 50,
                                    cluster = "nr")


controls_did_gt <- did_multiplegt(data = wagepan_df,
                                  variable_list = c("lwage",
                                                    "nr",
                                                    "year",
                                                    "union"),
                                  controls = "black",
                                  placebo = 2,
                                  breps = 50,
                                  cluster = "nr")



linear_trends_did_gt <- did_multiplegt(data = wagepan_df,
                                  variable_list = c("lwage",
                                                    "nr",
                                                    "year",
                                                    "union"),
                                  placebo = 2,
                                  breps = 50,
                                  cluster = "nr",
                                  trends_lin = "nr")

dynamic_effects_did_gt <- did_multiplegt(data = wagepan_df,
                                       variable_list = c("lwage",
                                                         "nr",
                                                         "year",
                                                         "union"),
                                       dynamic = 2,
                                       placebo = 2,
                                       breps = 50,
                                       cluster = "nr")




did_m <- no_control_did_gt$coefficents["union"]
did_pl_m <- controls_did_gt$coefficients["union"]
did_pl_m_2 <- linear_trends_did_gt$coefficients["union"]
did_pl_m_3 <- dynamic_effects_did_gt$coefficients["union"]






test_that("Table 4 Point Estimates Replicate", {
  expect_equal(fe_coef, 0.107)
  expect_equal(fd_coef, 0.06)
  expect_equal(did_m, 0.041)
  expect_equal(did_pl_m, 0.094)
  expect_equal(did_pl_m_2, -0.041)
  expect_equal(did_pl_m_3, -0.004)
})



test_that("Table 4 Std. Errors Replicate", {
  expect_equal(fe_sd, 0.03)
  expect_equal(fd_sd, 0.032)
  expect_equal(did_m_sd, 0.035)
  expect_equal(did_pl_m_sd, 0.038)
  expect_equal(did_pl_m_2_sd, 0.033)
  expect_equal(did_pl_m_3_sd, 0.033)
})
