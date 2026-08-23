pkgname <- "whomds"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('whomds')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("fig_density")
### * fig_density

flush(stderr()); flush(stdout())

### Name: fig_density
### Title: Plot a density of a score
### Aliases: fig_density

### ** Examples

fig_density(df_adults, score = "disability_score", cutoffs = c(19.1, 34.4, 49.6), 
x_lab = "Disability score")
fig_density(df_adults, score = "disability_score", var_color = "sex", 
cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")
fig_density(df_adults, score = "disability_score", var_color = "sex", 
var_facet = "age_cat",  cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")



cleanEx()
nameEx("fig_dist")
### * fig_dist

flush(stderr()); flush(stdout())

### Name: fig_dist
### Title: Plot a distribution of a score
### Aliases: fig_dist

### ** Examples

fig_dist(df_adults, score = "disability_score", score_cat = "disability_cat", 
cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")
fig_dist(df_adults, score = "disability_score", score_cat = "disability_cat", 
cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score", y_max = 2000)
fig_dist(df_adults, score = "disability_score", score_cat = "disability_cat", 
cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score", y_max = 0.2, pcent=TRUE)



cleanEx()
nameEx("fig_poppyramid")
### * fig_poppyramid

flush(stderr()); flush(stdout())

### Name: fig_poppyramid
### Title: Print a population pyramid
### Aliases: fig_poppyramid

### ** Examples

fig_poppyramid(df_adults, "age", "sex")



cleanEx()
nameEx("helper_indicator")
### * helper_indicator

flush(stderr()); flush(stdout())

### Name: helper_indicator
### Title: Create indicators from data frame
### Aliases: helper_indicator

### ** Examples

helper_indicator(df = df_adults, 
vars_indicators = c("EF1", "EF2", "EF3"), 
mapvalues_from = 1:5, 
mapvalues_to = c(0,0,0,1,1))



cleanEx()
nameEx("helper_rowSums")
### * helper_rowSums

flush(stderr()); flush(stdout())

### Name: helper_rowSums
### Title: Perform row sum
### Aliases: helper_rowSums

### ** Examples

x <- data.frame(v1 = c(NA,1:4), v2 = c(NA, 2:5), v3 = c(NA, 1:2, NA, 3))
helper_rowSums(x, na.rm = TRUE, allNA0 = TRUE)
helper_rowSums(x, na.rm = TRUE, allNA0 = FALSE)



cleanEx()
nameEx("table_basicstats")
### * table_basicstats

flush(stderr()); flush(stdout())

### Name: table_basicstats
### Title: Compute basic statistics of the number of members per group per
###   household
### Aliases: table_basicstats

### ** Examples

#create dummy table of household data, where each row represents one member
df_hh <- data.frame(HHID = sample(
  x = 1:300,
  size = 1000,
  replace = TRUE
),
age_cat = ordered(sample(
  x = c("18-24", "25-39", "40-64", "64-100"),
  size = 1000,
  replace = TRUE
)))
                
table_basicstats(df_hh, "HHID", "age_cat")



cleanEx()
nameEx("table_unweightedpctn")
### * table_unweightedpctn

flush(stderr()); flush(stdout())

### Name: table_unweightedpctn
### Title: Compute unweighted percent and N for multiple variables,
###   disaggregated
### Aliases: table_unweightedpctn

### ** Examples

table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"))
table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), 
group_by_var = "disability_cat")
table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), 
group_by_var = "disability_cat", spread_by_group_by_var = TRUE)



cleanEx()
nameEx("table_weightedpct")
### * table_weightedpct

flush(stderr()); flush(stdout())

### Name: table_weightedpct
### Title: Calculate table of percentages or N of response distribution for
###   survey items, survey weighted, disaggregated
### Aliases: table_weightedpct

### ** Examples

table_weightedpct(df_adults, 
    vars_ids = c("HHID", "PSU"),
    vars_strata = "strata",
    vars_weights = "weight",
    formula_vars = paste0("EF",1:10),
    formula_vars_levels = 1:5,
    by_vars = "sex")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
