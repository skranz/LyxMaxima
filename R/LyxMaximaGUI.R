
#' A list that contains GUI values
new.lyma = function(txt=character(0)) {
  EMPTY.MAT = matrix(NA,nrow=0,ncol=2)
  
  lyma = list(txt = txt,ma.var="",def = EMPTY.MAT,simp.pat = character(0),simp.pat.org = character(0), subst= EMPTY.MAT,assum = character(0), fun = character(0), eq = character(0))
  lyma$simp.pat.org = "gcfac(factor())";
  lyma$simp.pat = convert.patterns(lyma$simp.pat.org);
  lyma
}

button.go.fun = function() {  
  txt = readClipboard(format = 1, raw = FALSE)  
  restore.point("button.go.fun")
  svalue(text.term)=txt
  lyma = gui.to.lyma()
  lyma$txt = txt
  write.text(txt,"temp_lyx_math.txt")	
  ret = lyx.go(lyma = lyma)
  if (!is.null(ret)) {
    writeClipboard(ret)
    svalue(text.term)=ret
  }
} 

button.equal.fun = function() {
  txt = readClipboard(format = 1, raw = FALSE)
  kill.all()
  write.text(txt,"temp_lyx_math.txt")	
  svalue(text.term)="Check equality..."
  svalue(text.term)=txt
  lyma = gui.to.lyma()
  svalue(text.term)=paste0(lyx.is.equal(txt,lyma=lyma)," ", round(runif(1,0,1000)))
}


button.solve.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.solve.fun")
  write.text(txt,"temp_lyx_math.txt")	
  lyma = gui.to.lyma()
  #Rprof(tmp <- tempfile())
  ret = lyx.solve(txt,lyma=lyma)
  #Rprof()
  #summaryRprof(tmp)
  writeClipboard(ret)
  svalue(text.term)=ret
}


button.diff.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.diff.fun")
  svalue(text.term)=paste("Differentiate...",paste(txt,collapse="\n"),sep="\n")
  write.text(txt,"temp_lyx_math.txt")	
  lyma = gui.to.lyma()
  ret = lyx.diff(txt,lyma=lyma)
  writeClipboard(ret)
  svalue(text.term)=ret
}


button.sign.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.diff.fun")
  svalue(text.term)=paste("Determine sign of...",paste(txt,collapse="\n"),sep="\n")
  write.text(txt,"temp_lyx_math.txt")	
  lyma = gui.to.lyma()
  ret = lyx.sign(txt,lyma=lyma)
  svalue(text.term)=ret
}


button.clear.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.clear.fun")
  kill.all()
  svalue(text.term)="Maxima has been cleared!"
}

button.read.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.diff.fun")
  ret = convert.maxima.output()
  txt = ret$txt
  #txt = ret$txt[ret$math.rows]
  writeClipboard(txt)
  svalue(text.term)=txt
}


button.revsubst.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.diff.fun")
  #rerestore.point("button.diff.fun")
  lyma = gui.to.lyma()  
  txt = do.reverse.subst(txt,lyma)
  writeClipboard(txt)
  svalue(text.term)=txt
}


button.R.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.R.fun")
  #rerestore.point("button.R.fun")
  svalue(text.term)=txt
  lyma = gui.to.lyma()	
  txt = convert.math(txt,lyma=lyma)
  writeClipboard(txt)
  svalue(text.term)=txt
}

# Saves current lyma
button.save.fun  = function() {
  lyma = gui.to.lyma()
  dump("lyma","lyma.R")
}

load.lyma = function(file="lyma.R") {
  
  source(file,local=TRUE)
  lyma.to.gui(lyma)
  #if (!is.null(lyma$win.size))
  #  size(win)<- lyma$win.size
}

# Transforms a simple R or Maxima expression to Lyx
ma.to.ly.code = function(txt,lyma=new.lyma()) {
  txt = paste("printf(stream,",'"~a", tex(',txt,',false));',sep="")
  sep = "\n newline(stream); \n"
  txt = paste(txt,collapse=sep)
  txt = c(txt,"\n newline(stream); \n")
  eval.mao.to.ly(txt,lyma)
}

button.lyx.fun = function() {	
  txt = readClipboard(format = 1, raw = FALSE)
  restore.point("button.lyx.fun")
  #rerestore.point("button.lyx.fun")
  
  kill.all()
  svalue(text.term)=txt
  lyma = gui.to.lyma()
  txt = ma.to.ly.code(txt,lyma=lyma)
  writeClipboard(txt)
  svalue(text.term)=txt
}


draw.window = function() {
  win  <<-  gwindow("Lyx CAS",  visible=FALSE, width=150, height=80)
  nb <<- gnotebook(container=win)
  
  
  # Groups for the different pages
  group.main <<- ggroup(container=nb,horizontal=FALSE)
  group.code  <<- ggroup(container=nb,horizontal=FALSE)
  group.subst  <<- ggroup(container=nb,horizontal=FALSE)
  group.assum  <<- ggroup(container=nb,horizontal=FALSE)
  group.fun  <<- ggroup(container=nb,horizontal=FALSE)
  
  length(nb)
  names(nb) <- c("Main","Code","Substitute","Assume","Functions")
  svalue(nb) <- 1
  
  
  # Main page
  group.button <<- ggroup(container = group.main, height=10)
  group.button2 <<- ggroup(container = group.main, height=15)
  group.button3 <<- ggroup(container = group.main, height=20)
  #group.button4 <<- ggroup(container = group.main, height=20)
  group.txt <<- ggroup(container = group.main,horizontal=FALSE)
  
  button.go <<- gbutton("Go",container=group.button, width=30, handler  = function(h,...) { button.go.fun()} )
  #button.convert <<- gbutton("File",container=group.button, handler  = function(h,...) {button.convert.fun()} )
  button.solve <<- gbutton("solve",container=group.button, handler  = function(h,...) {button.solve.fun()} )	
  button.diff <<- gbutton("diff",container=group.button, handler  = function(h,...) {button.diff.fun()} )	
  button.equal <<- gbutton("equal?",container=group.button, handler  = function(h,...) {button.equal.fun()} )	
  
  button.sign <<- gbutton("sign?",container=group.button2, handler  = function(h,...) {button.sign.fun()} )	
  
  button.R <<- gbutton("to R",container=group.button2, handler  = function(h,...) {button.R.fun()} )	
  button.lyx <<- gbutton("to Lyx",container=group.button2, handler  = function(h,...) {button.lyx.fun()} )	
  #button.clear <<- gbutton("clear",container=group.button2, handler  = function(h,...) {button.clear.fun()} )	
  button.revsubst <<- gbutton("unsubst",container=group.button3, handler  = function(h,...) {button.revsubst.fun()} )	
  button.read <<- gbutton("read",container=group.button3, handler  = function(h,...) {button.read.fun()} )	
  button.save <<- gbutton("save",container=group.button3, handler  = function(h,...) {button.save.fun()} )  
  #button.save <<- gbutton("",container=group.button3, handler  = function(h,...) {button.save.fun()} )  
  
  
  text.term <<- gtext("",container=group.txt,height=50)
  
  label.simp.main <<- glabel("simplify command:", container=group.txt)
  
  text.simp.main <<- gtext("gcfac(factor());",container=group.txt, handler  = function(h,...) {})	
  
  label.var.main <<- glabel("variable (solve or diff):", container=group.txt)
  text.var.main <<- gtext("",container=group.txt)	
  
  
  size(button.go)=20
  size(button.equal)=35
  size(button.solve)=27
  size(button.diff)=22
  size(button.sign)=30
  size(button.read)=30
  size(button.R)=25
  size(button.lyx)=28
  #size(button.clear)=22
  size(button.revsubst)=32
  size(button.save)=30
  
  size(text.term) = c(250,80)
  size(text.simp.main)=c(250,50)
  size(text.var.main)=c(250,30)
  
  # Other pages
  label.code <<- glabel("Maxima code to run:", container=group.code)
  text.code <<- gtext("",container=group.code)
  label.subst <<- glabel("Substitions (LaTex format):", container=group.subst)
  text.subst <<- gtext("",container=group.subst)
  label.subst <<- glabel("Assumptions (Maxima format):", container=group.assum)
  text.assum <<- gtext("",container=group.assum,height=30)
  label.fun <<- glabel("Functions (Maxima format):", container=group.fun)
  text.fun <<- gtext("",container=group.fun,height=30)
  
  
  gui.to.lyma <<- function(lyma = new.lyma()) {
    lyma$txt = svalue(text.term);
    lyma$subst = eq.to.leftright.mat(svalue(text.subst));		
    
    lyma$simp.pat.org = svalue(text.simp.main);
    lyma$simp.pat = convert.patterns(lyma$simp.pat.org);
    lyma$code = svalue(text.code);
    
    lyma$assum = svalue(text.assum);
    lyma$fun = svalue(text.fun);
    lyma$fun = sep.lines(lyma$fun,"\n");
    lyma$fun = merge.lines(lyma$fun,";");
    lyma$fun = sep.lines(lyma$fun,";");
    lyma$ma.var = svalue(text.var.main);
    
    lyma$win.size = size(win)
    return(lyma);
  }
  
  lyma.to.gui <<- function(lyma) {
    svalue(text.term)=lyma$txt;
    svalue(text.simp.main)=lyma$simp.pat.org;
    svalue(text.code) = lyma$code ;
    svalue(text.subst)=paste(lyma$subst[,1],"=",lyma$subst[,2],collapse="\n");
    svalue(text.assum)=lyma$assum;
    svalue(text.fun)=lyma$fun;
  }
  
  visible(win) <- TRUE
  
  # Place window always on top
  tcl("wm", "attributes", getToolkitWidget(win), topmost = 1)
  
  #focus <- win
}
#draw.window()

