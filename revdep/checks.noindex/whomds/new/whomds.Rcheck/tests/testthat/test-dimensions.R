context("Dimensions of tables")

test_that("Dimensions as number of levels and 4", {
  tab <- table_unweightedpctn(df_adults, c("sex", "edu_cat"))
  
  expect_equal(nrow(tab),
               length(c(
                 levels(df_adults$sex), levels(df_adults$edu_cat)
               )))
  expect_equal(ncol(tab), 4)
})


test_that("Dimensions as multiple of formula_vars, levels, and by_vars", {
  
  formula_vars <- paste0("EF",1:10)
  formula_vars_levels <-1:5
  by_vars <- "sex"
  
  tab <- table_weightedpct(df_adults, 
                           vars_ids = c("PSU","HHID"),
                           vars_strata = "strata",
                           vars_weights = "weight",
                           formula_vars = formula_vars,
                           formula_vars_levels = formula_vars_levels,
                           by_vars = by_vars)
  
  expect_equal(nrow(tab),
               length(formula_vars) * length(formula_vars_levels) * length(levels(dplyr::pull(df_adults,by_vars)))
  )
  expect_equal(ncol(tab), 4)
  
  
  
}
)
