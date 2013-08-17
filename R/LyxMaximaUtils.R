
#' Warning function that will display warning in LyxMaxima GUI
mywarning = function(txt) {
  warning(txt)
  svalue(text.term) = paste(txt,"\n",svalue(text.term))
}  

#' is.null(x) | length(x) == 0
is.empty = function(x) {
  is.null(x) | length(x) == 0
}
