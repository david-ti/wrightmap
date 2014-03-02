wrightMap.character <-
function(show, p.est, p.type = NULL, ...) {
    model <- CQmodel(show, p.est, p.type)
    wrightMap(model, ...)
}
