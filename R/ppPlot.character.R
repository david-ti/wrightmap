ppPlot.character <-
function(thetas, thresholds, p.type = NULL, ...) {
    model <- CQmodel(thetas, thresholds, p.type)
    ppPlot(model, ...)
}