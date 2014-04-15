wmap.shiny <- function() {
	fpath <- system.file("extdata", package="WrightMap")
	runApp(fpath)
}