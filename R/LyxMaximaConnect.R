#' Returns enviornment with global variables
get.LyxMax = function() {
  glob()
}

set.glob = function(...) {
  args = list(...)
  copy.into.env(dest=glob(),source=args)
}
glob = function() {
  .LyxMaxima.Env
}



init.LyxMaxima = function(...) {
  library(tcltk)
  library(tcltk2)
  library(gWidgets)
  library(gWidgetstcltk)
  options(guiToolkit="tcltk")
  
  library(stringtools)
  library(restorepoint)
  library(RMaxima)
  
  # Set package global variables
  greek = c("alpha","beta","gamma","delta","varepsilon","epsilon","zeta","eta",
            "theta","iota","kappa","lambda","mu","nu","xi","omicron","pi",
            "rho","sigma","tau","phi","chi","psi","omega",
            "Beta","Gamma","Delta","Theta","Lambda","Xi","Pi",
            "Sigma","Phi","Chi","Psi","Omega"
  )
  set.glob(
    greek = greek,  		
    greek.or = paste("(",str.list.to.regexp.or(greek,fixed=FALSE),")",sep=""),
    txt.out = "",
    EMPTY.POS = matrix(NA,nrow=0,ncol=2),
    EMPTY.IND = matrix(NA,nrow=0,ncol=2),
    MAXIMA.PIPE = NULL
  )
  set.LyxMaxima.paths(...)
}

set.LyxMaxima.paths = function(
  LYXCAS.PATH = paste(find.package("LyxMaxima"),"/",sep=""),
  LYX.PATH    = "", # Assume that it is in PATH
  OUT.PATH    = get.mx()$outpath,
  LYX.EXE     = paste(LYX.PATH,"lyx.exe",sep=""),
  LYX.OUTFILE.NAME  = "LyxCASResult",
  MAXIMA.HEADER = paste(LYXCAS.PATH,"maxima_script_header.mac",sep=""),
  LYX.HEADER = paste(LYXCAS.PATH,"maxima_lyx_header.txt",sep=""),
  LYX.FOOTER = paste(LYXCAS.PATH,"maxima_lyx_footer.txt",sep="")
) {
  copy.into.env(source=nlist(LYXCAS.PATH,LYX.PATH,OUT.PATH,LYX.EXE,LYX.OUTFILE.NAME,MAXIMA.HEADER,LYX.HEADER,LYX.FOOTER),dest=glob())
}


#' Writes a maxima header for LyxMaxima
write.maxima.header = function() {
  #open(max.pipe)  
  header = read.text(glob()$MAXIMA.HEADER,merge = FALSE)
  header = c(header,
             'set_tex_environment_default ("?#S#", "?");'
             #'set_tex_environment_default ("\n $", "$");'
  )
  mx.write(header)
}

#' Clear all declarations in Maxima but reload header
kill.all = function() {
  writeLines("kill(allbut(pullsign,postrans));",MAXIMA.PIPE)
  #write.maxima.header()
}


start.LyxMaxima = function(..., init=TRUE) {
  start.maxima(...)
  if (init)
    init.LyxMaxima(...)
  write.maxima.header()
  draw.window()
}

send.to.maxima.and.lyx = function(txt,lyma=new.lyma()) {
  ret = send.to.maxima(txt,lyma)
  ret = call.lyx()  
}

send.to.maxima = function(txt,lyma=new.lyma()) {  
  restore.point("send.to.maxima.and.lyx")
  if (length(lyma$assum)>0) {
    if(nchar(lyma$assum)>0) {
      txt = c(txt,paste('assume(',lyma$assum,');'))
    }
  }
  # Use RMaxima function
  call.maxima(txt)
}

call.lyx = function(maxima.outfile= get.mx()$outfile, lyx.outfile.name= glob()$LYX.OUTFILE.NAME) {
  restore.point("call.lyx")
  
  # Read the generated file and adapt the Maxima Tex output, which unfortunately is quite ugly
  ret = convert.maxima.output(maxima.outfile)
  
  if (length(ret$txt)==0) {
    mywarning("Maxima has written no output....")
    return()
  }
  txt = ret$txt
  math.rows = ret$math.rows
  txt[math.rows] = paste('\n \\end_layout \n \\begin_layout Standard \n \\begin_inset Formula \n \\begin{multline*} \n',
                         txt[math.rows],
                         '\n \\end{multline*} \n \\end_inset \n')
  
  txt = sep.lines(txt)
  # Layout commands  
  rows = which(str.left(txt,9)=="##Layout:")
  if (length(rows)>0) {
    layout = str.remove.ends(txt[rows],9,0)
    txt[rows] = paste('\n \\end_layout \n \\begin_layout ',layout,'\n',sep="")
  }
  
  
  txt = paste(txt,collapse="\n")
  
  header = read.text(LYX.HEADER,merge=TRUE)
  footer = read.text(LYX.FOOTER,merge=TRUE)
  
  str = paste(header,txt,footer,collapse="\n")
  
  
  out.path = glob()$OUT.PATH
  lyx.exe = paste('"',glob()$LYX.EXE,'"',sep="")
  
  lyx.file = paste(out.path,"/",lyx.outfile.name,".lyx",sep="")
  write.text(str,lyx.file)
  
  
  lyx.file = paste(OUT.PATH,"/",lyx.outfile.name,sep="")
  # Open created file in Lyx
  command = paste(lyx.exe,' "',lyx.file,'"',sep="") 
  system(command,intern = !TRUE,wait=FALSE,ignore.stdout = FALSE, ignore.stderr = FALSE)
  
  # current.dir = getwd()
  # lyx.pipe = '\\\\.\\pipe\\lyxpipe.in'
  # lyx.file = paste(current.dir,'/NEBENRECHNUNG.lyx',sep="")
  # command = paste('echo "LYXCMD:lyx-script:file-open:',lyx.file,'" > ',lyx.pipe,sep="");
  # command = paste('echo LYXCMD:lyx-script:file-open:',lyx.file,' > ',lyx.pipe,sep="");
  # command
  # system(command,intern = FALSE,wait=TRUE,ignore.stdout = FALSE, ignore.stderr = FALSE)
}
