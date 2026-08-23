pkgname <- "irtGUI"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('irtGUI')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("irtGUI")
### * irtGUI

flush(stderr()); flush(stdout())

### Name: irtGUI
### Title: Item Response Theory Analysis with a user-frindly Graphic User
###   Interface.
### Aliases: irtGUI

### ** Examples

if(interactive()){
## Run this code for launching the Graphic User Interface
irtGUI()
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
