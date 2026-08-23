pkgname <- "irtawsi"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('irtawsi')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("irtawsi")
### * irtawsi

flush(stderr()); flush(stdout())

### Name: irtawsi
### Title: Items Response Theory Analysis with Steps and Interpretation
### Aliases: irtawsi

### ** Examples

if(interactive()){
## Run this code for launching the Graphic User Interface
irtawsi()
}



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
