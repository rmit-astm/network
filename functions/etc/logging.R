
echo<- function(msg) {
  cat(paste0(as.character(Sys.time()), ' | ', msg))  
}

printProgress<-function(row, total_row, char) {
  if((row-50)%%2500==0) echo('')
  cat('.')
  if(row%%500==0) cat('|')
  if(row%%2500==0) cat(paste0(char,' ', row, ' of ', total_row, '\n'))
}

# runs a step (eg 'x <- f(...)') in the calling environment and logs how long
# it took, named by the function it calls
timeStep <- function(expr) {
  step <- substitute(expr)
  if (is.call(step) && identical(step[[1]], as.name("<-"))) step <- step[[3]]
  elapsed <- system.time(expr)[["elapsed"]]
  echo(sprintf("%s took %.1f min\n", deparse(step[[1]]), elapsed / 60))
}
