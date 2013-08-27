#' A small GUI for Computer Algebra within Lyx

#' Checks whether a latex equality holds true
#' @export
lyx.is.equal = function(txt, lyma) {
  restore.point("check.equality")
  
  txt = merge.lines(txt)
  lr = str.split(txt,"=")[[1]][1:2]
  l =  convert.math(lr[1],lyma=lyma)
  r =  convert.math(lr[2],lyma=lyma)

  out = mx.is.equal(l,r)
  return(out)
}

examples.lyx.is.equal = function() {
  lyx.is.equal("x_{1}+x_{1}=2x_{1}")
  lyx.is.equal("x_{1}+x_{1}=x_{1}")
  #lyma = new.lyma()
  #lyx.is.equal("-\frac{\bar{\sigma}-\beta\sigma_{b}}{\beta-1}=\frac{\bar{\sigma}-\beta\sigma_{b}}{1-\beta}")
}

#' Lyx code to maxima solve code
#' 
#' @param str a Latex math expression
#' @param var a Maxima variable name
ly.mao.solve = function(str,ma.var=lyma$ma.var,lyma=new.lyma()) {
  restore.point("ly.mao.solve")
  
  str = str.replace(str,"$","")
  row.eq = has.substr(str,"=")
  str = str[row.eq]
  
  num.eq = NROW(str)
  str = convert.math(str,lyma=lyma)

  var = ma.var
  # Find variables as left hand side of equations (need to change later)
  if (nchar(var)==0) { 
    var = substring(str,1,eq.pos-1)
    eq.pos = str.find(str,"=",first=TRUE,simplify=TRUE)
  } else {
    var = sep.lines(var,",")
  }

  # Example: solve([x^2+y=10*z, x-y=3*z], [x,y]);
  out.eq = paste(str,collapse=",\n")
  out.var = paste(var,collapse=",")
  
  out = paste("sol:solve([",out.eq,"],[",out.var,"]);",sep="")
  c(out,
    ma.mao.simplify(term=paste("sol[1]",sep=""),lyma = lyma))
}
examples.ly.mao.solve = function() {
  ly.mao.solve("x_{1}+y=5","x_1")
}


#' diff: lyx to mao code
#'
#' @param str a Latex math expression
#' @param ma.var a Maxima variable name
ly.mao.diff = function(str,ma.var=lyma$ma.var,lyma=new.lyma()) {
  restore.point("ly.mao.diff")
    
  str = str.replace(str,"$","")
  str = convert.math(str,lyma=lyma)
  
  
  var = sep.lines(ma.var,",")
  
  # Example: solve([x^2+y=10*z, x-y=3*z], [x,y]);
  out.term = paste(str,collapse=",\n")
  
  out = paste("sol:diff(",out.term[1],",",var[1],");",sep="")
  c(out,
    ma.mao.simplify(term=paste("sol",sep=""),lyma = lyma))
}
examples.ly.mao.diff = function() {
  ly.mao.diff("x_{1}^2+5","x_1")
}

#' Maxima code for checking sign of a lyx math expression
ly.mao.sign = function(str,lyma=new.lyma()) {
  restore.point("ly.mao.sign")  
  
  str = str.replace(str,"$","")
  str = convert.math(str,lyma=lyma)
  
  str = paste("sign(",str,")");
  txt = paste("printf(stream,",'"~a",',str,');',sep="")
  sep = "\n newline(stream); \n"
  txt = paste(txt,collapse=sep)
  txt = c(txt,"\n newline(stream); \n")

  return(txt)
}
examples.ly.mao.sign = function() {
  ly.mao.sign("x_{1}^2+1")
}

#' Convert maxima code in maxima code generation code
ma.mao.code = function(str, code.file = paste(tempdir(),'/maxima_code.mac',sep="") ) {
  restore.point("ma.mao.code")
  
  str = merge.lines(str,";")
  str = str.trim(sep.lines(str,";"))
  str = str[str!=""]
  # As a backup to see the code later
  write.text(paste(str,";"),code.file)
  
  # Loop through every line of code
  if (length(str)==0)	return("");
  
  max.code = NULL
  code.li = lapply(str, function(code) {    
    assign.pos = str.find(code,":",simplify=TRUE)
    if (!has.substr(code,":")) {
      return(c(mao.comment(code),ly.mao.tex(code)))
    } else {
      # An assigmnet like x: 5*t^2
      left = str.split(code,":",first=TRUE)[1]
      return(c(
        paste(code,";",sep=""),
        mao.comment(code),
        ly.mao.tex(left)
      ))
    }
  })
  do.call("c",code.li)
}


#' Takes maxima code and transforms it into a simplification command
ma.mao.simplify = function(term,lyma=new.lyma()) {
  restore.point("ma.mao.simplify")
  txt =str.replace(lyma$simp.pat,"[TERM]",term)	
  if (txt=="") {
    txt = term
  }
  txt = paste("printf(stream,",'"~a", tex(',txt,',false));',sep="")
  sep = "\n newline(stream); \n"
  txt = paste(txt,collapse=sep)
  txt = c(txt,"\n newline(stream); \n")

  return(txt)
}

examples.ma.mao.simplify = function() {
  ma.mao.simplify("x1+x1+x1")
}

mao.comment=function(txt, layout="Standard") {
  sep = "\n newline(stream); \n"
  txt = paste('printf(stream,"~a","', txt,'");' , collapse=sep)
  
  layout.txt = paste('printf(stream,"~a","', '##Layout:', layout,'");', sep="")
  c(layout.txt,sep,txt,sep)
}

ly.mao.tex=function(txt) {
  sep = "\n newline(stream); \n"
  txt = paste("\n printf(stream,",'"~a", tex(',txt,",false));",sep="")
  txt = paste(txt,collapse=sep)
  c(txt,sep)
}

#' Determines the sign of a lyx expression
#' @export
lyx.sign = function(txt=lyma$txt,lyma=new.lyma(txt)) {
  restore.point("lyx.sign")
  
  kill.all()
  
  ly.mao.sign(txt,lyma=lyma)
  ret.max = send.to.maxima(txt.out,lyma=lyma)
  ret = read.maxima.out(ret.max$out.file)[1]
  sign.names = list(pnz="sign unknown",pn="not 0",pz="positive or zero",pos="strictly positive",neg="strictly negative",nz="negative or zero")
  if (ret %in% names(sign.names))
    ret = sign.names[[ret]]
  
  return(ret)
}



#' Differentiates a lyx expression
#' @export
lyx.diff = function(txt=lyma$txt,ma.var=lyma$ma.var,lyma=new.lyma(txt)) {
  restore.point("lyx.diff")  
  kill.all()
  mao.code = ly.mao.diff(txt,ma.var=ma.var,lyma=lyma)
  eval.mao.to.ly(mao.code,lyma=lyma)
}

examples.lyx.diff = function() {
  txt = "x^2"
  lyx.diff("x^2","x")
}

#' Solves a lyx equation
lyx.solve = function(txt=lyma$txt,ma.var = lyma$ma.var,lyma=new.lyma(txt)) {
  restore.point("lyx.solve")  
  kill.all()
  mao.code = ly.mao.solve(txt,ma.var=ma.var,lyma=lyma)
  eval.mao.to.ly(mao.code,lyma=lyma)
}

examples.lyx.solve = function() {
  lyx.solve("x^2+1+y=0","x")
}

#' Simplifies a lyx expression
lyx.simplify = function(txt=lyma$txt,lyma=new.lyma(txt)) {
  restore.point("lyx.simplify")
  #rerestore.point("lyx.simplify")
  
  kill.all()
  txt = convert.math(txt,lyma=lyma)
  mao.code = ma.mao.simplify(txt,lyma=lyma)
  eval.mao.to.ly(mao.code,lyma=lyma)[1]
}
examples.lyx.simplify = function() {
  lyx.simplify("x^2+2*x+1")
}

lyx.add.subst = function(str,lyma=new.lyma()) {
  restore.point("lyx.add.subst")
  str = str[str!=""]
  mat = eq.to.leftright.mat(str)
  if (length(lyma$subst)>0) {
    # Overwrite existing definitions with same name
    lyma$subst = lyma$subst[! lyma$subst[,1] %in% mat[,1],, drop=FALSE]
    lyma$subst = rbind(mat,lyma$subst)
  } else {
    lyma$subst = mat
  }
  lyma.to.gui(lyma)
  lyma  
}

lyx.matex = function(str,lyma=new.lyma()) {
  restore.point("lyx.matex")
  str = sep.lines(str, ";")
  str = str.replace(str,";","")
  str = merge.lines(str,";")
  ma.code = matex.to.ma.code(str,lyma=lyma) 
  str = sep.lines(ma.code, ";")
  
  ma.header = str[-length(str)]
  ma.header = paste0(merge.lines(ma.header,";"),";")
  send.to.maxima(ma.header,lyma=lyma)
  
  ma.last = ma.mao.code(str[length(str)]);
  ret = eval.mao.to.ly(ma.last,clear=FALSE)
  lyma$txt = ret
  lyma.to.gui(lyma)
  ret  
}

#' txt is some Lyx-Latex code copied from view Latex source
#' 
#' The code can contain some commands starting with #
#' @export
lyx.go = function(txt=lyma$txt,lyma = new.lyma(txt),file=NULL) {
  restore.point("read.lyx.math")
  #rerestore.point("read.lyx.math")
  
  if (!is.null(file)) {
    txt = read.text(file,merge=FALSE)
  }
  txt = str.trim(txt)
    
  # REMOVE TRASH LINES
  txt = merge.lines(txt)
  txt = str.replace(txt,'\\end{','\n\\end{')
  txt = sep.lines(txt)
  trash.lines = str.list.to.regexp.or(c('\\begin{','\\end{'))
  rows = has.substr(txt,trash.lines,fixed=FALSE)
  txt  = txt[!rows]
  
  # Change \texttt{...} lines
  #rows = str.left(txt,8)=="\\texttt{" & str.right(txt,1)=="}"
  #txt[rows] = str.remove.ends(txt[rows],8,1)
  
  txt = str.replace(txt,"~"," "); # Somehow this appears in Lyx Code mode
  txt = str.replace(txt,"{*}","*"); # Somehow this appears in Lyx Code mode
  txt = str.replace(txt,"{[}","["); # Somehow this appears in Lyx Code mode
  txt = str.replace(txt,"{]}","]"); # Somehow this appears in Lyx Code mode
  txt = str.replace(txt,"\\%","%")
  
  #txt = str.replace(txt,"$","")
  
  txt = txt[txt!=""]
  
  #FIND AND REMOVE COMMENT ROWS 
  comment.rows = which(str.left(txt,3)=="\\#'" | str.left(txt,2)=="#'")
  #txt = txt[!comment.rows]
  
  
  #FIND COMMAND BLOCKS
  crows = which((str.left(txt,2)=="\\#" | str.left(txt,1)=="#"))
  crows = setdiff(crows,comment.rows)
  
  # No commands found: simplify the math
  if (length(crows)==0) {
    return(lyx.simplify(txt,lyma));
  }
  
  crows.slash = which(str.left(txt,2)=="\\#")
  txt[crows.slash] = str.remove.ends(txt[crows.slash],1,0)
  
  com = txt[crows]
  com.start = crows
  com.end = c(crows[-1],NROW(txt)+1)-1
  
  com.i = 1
  count.subst = 0

  ret.txt = ""
  while(com.i <= NROW(com)){
    rows = com.start[com.i]:com.end[com.i]		
    
    str = txt[rows]
    
    # Only keep only the part after command in first row
    space.pos = str.find(str," ",first=TRUE,simplify=TRUE)
    if (length(space.pos)>0) {
      str[1] = substring(str[1],space.pos+1,nchar(str[1]))
      if (is.na(str[1])) 
        str[1]=""
    } else {
      str[1]=""
    }
    first.line = str[1]
    # Replace ";" with "\n" and split again
    str = merge.lines(str);
    str = str.replace(str,";","\n");
    rows = str.replace(str,"%\n","\n")
    
    str = sep.lines(str);
    
    
    # REMOVE TRASH LINES
    trash.lines = str.list.to.regexp.or(c('\\begin{','\\end{'))
    rows = has.substr(str,trash.lines,fixed=FALSE)
    str  = str[!rows]
    
    
    #FIND AND REMOVE COMMENT ROWS 
    if (str.left(com[com.i],7)!="#MAXIMA" & str.left(com[com.i],2)=="#R") {
      comment.rows = (str.left(str,3)=="\\#'" | str.left(str,2)=="#'")
      str = str[!comment.rows]
    }
    
    str.backup = str
    if (str.starts.with(com[com.i],"#SUBST")) {
      lyma=lyx.add.subst(str,lyma)
      ret.txt = paste(ret.txt,paste0("\n#SUBST\n",
      paste0(lyma$subst[,1],"=",lyma$subst[,2],collapse="\n")))
    }	
    else if (str.starts.with(com[com.i],"#SIMP.PATTERN")) {
      #stop()
      simp.patterns <- convert.patterns(str)
      lyma$simp.pat <- simp.patterns
      lyma.to.gui(lyma)
      ret.txt = paste(ret.txt,paste0("\n#SIMP.PATTERN\n",
        paste0(lyma$sim.pat,collapse="\n")))
      
    }
    else if (str.starts.with(com[com.i],"#ASSUME")) {
      #stop()
      lyma$assum <- convert.assumptions(str)
      lyma.to.gui(lyma)
      ret.txt = paste(ret.txt,paste0("\n#ASSUME\n",paste0(lyma$assume,collapse="\n")))
    }
    else if (str.starts.with(com[com.i],"#FUN")) {
      #stop()
      str = str[str!=""]
      str = convert.math(str,final.curley = "remove",lyma=lyma)
      lyma$fun <- str
      lyma.to.gui(lyma)
      ret.txt = paste(ret.txt,paste0("\n#FUN\n",paste0(lyma$fun,collapse="\n")))
    } 		 		
    else if (str.starts.with(com[com.i],"#MAXIMA")) {
      lyma.to.gui(lyma)
      str = sep.lines(str, ";")
      str.replace(str,";","")
      str = merge.lines(str,";")
      ma.code = matex.to.ma.code(str) 
      return(ma.code)
    }
    else if (str.starts.with(com[com.i],"#MATEX")) {
      lyma.to.gui(lyma)
      ret = lyx.matex(str,lyma)
      return(ret)
    }
    else if (str.starts.with(com[com.i],"#R")) {
      #str = txt[rows]
      lyma.to.gui(lyma)
      return(matex.to.ma.code(str))
    } 
    else if (str.starts.with(com[com.i],"#END")) {
      break
    }	else {
      mywarning(paste("Unknown command", com[com.i], ". Commands must be in CAPITAL letters"))
    }
    com.i=com.i+1
  }
  return(ret.txt)
}

