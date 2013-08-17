#' A small interface for Computer Algebra within Lyx
#' The programm typically works as follows:
#' 1. reads mathematical formulas in Latex format that are in the clipboard
#' 2. translates them into Maxima formulas
#' 3. some Maxima code is added, like simplification and code that makes Maxima write the results into a file (that is because socket connection or fifo pipes, seem not to work correctly on my Windows computer)
#' 4. this code is piped to Maxima and executed
#' 5. the results of maxima are read and converted into suitable latex code
#' 6. the results are written into the clipboard
#' 7. the user can paste them back into Lyx

#' Variables can have 3 main representations:
#' 1. Latex original : y, u_{1}(x_{L}), \alpha_{1}, \alpha_{\beta}^{2}(\gamma,x,\delta)
#' 2. Latex canonical: y, u_{1L}      , \alpha_{1}, \alpha_{\beta2\gamma x\delta}
#' 3. Maxima         : y, u_1L        , alpha_1   , alpha_beta2gammaxdelta

#' The user has to specify a canonical form for every variable that appears and is not
#' in canonical form in the section #DEFINE
#' We can assign several Latex original versions to one Latex canonical
#' Eg. u_{1}^{L} and u_{1}(x_{L}) can both have form u_{1L}
#' But every Latex original must have a unique canonical
#' The program creates a one-to-one mapping between Latex canonical and Maxima 

.onAttach = function(...)  {
  init.LyxMaxima()
  cat("\nrun start.LyxMaxima()") 
  #start.LyxMaxima(init=FALSE)
}

.LyxMaxima.Env = new.env(parent=.GlobalEnv) 

#' Function when check.equality button has been pressed
check.equality = function(txt) {
  restore.point("check.equality")
  #rerestore.point("check.equality")
  
  txt = merge.lines(txt)
  lr = str.split(txt,"=")[[1]][1:2]
  lr = convert.math(lr)
  
  com = paste('is(equal(',lr[1],",",lr[2],'))',sep="")
  #com = c(paste('is(equal(gcfac(factor(',lr[1],"-(",lr[2],')),0))'),com)
  
  com = c("prederror : false;",
          paste('printf(stream,"~a",', com ,');',sep=""))
  ret = call.maxima(com)
  out = read.maxima.out(ret$out.file)
  return(out)
}

add.solve = function(str,var="", make.lyx.file = TRUE,lyma=new.lyma()) {
  restore.point("add.solve")
  
  str = str.replace(str,"$","")
  row.eq = has.substr(str,"=")
  str = str[row.eq]
  
  num.eq = NROW(str)
  str = convert.math(str,lyma=lyma)
  
  # Find variables as left hand side of equations (need to change later)
  if (nchar(var)==0) { 
    var = str.substr(str,1,eq.pos-1)
    eq.pos = str.find(str,"=",first=TRUE,simplify=TRUE)
  } else {
    var = sep.lines(var,",")
  }
  
  # Example: solve([x^2+y=10*z, x-y=3*z], [x,y]);
  out.eq = paste(str,collapse=",\n")
  out.var = paste(var,collapse=",")
  
  out = paste("sol:solve([",out.eq,"],[",out.var,"]);",sep="")
  add(out)
  add.simplify(term=paste("sol[1]",sep=""),lyma = lyma)
}


add.diff = function(str,var="",lyma=new.lyma()) {
  restore.point("add.diff")
  
  #stopifnot(!make.lyx.file)
  
  str = str.replace(str,"$","")
  str = convert.math(str,lyma=lyma)
  
  var = sep.lines(var,",")
  
  # Example: solve([x^2+y=10*z, x-y=3*z], [x,y]);
  out.term = paste(str,collapse=",\n")
  
  out = paste("sol:diff(",out.term[1],",",var[1],");",sep="")
  add(out)
  add.simplify(term=paste("sol",sep=""),lyma=lyma)
}

# Check the sign of an expression
add.sign = function(str,lyma=new.lyma()) {
  restore.point("add.sign")  
  #stopifnot(!make.lyx.file)
  
  str = str.replace(str,"$","")
  str = convert.math(str,lyma=lyma)
  
  str = paste("sign(",str,")");
  txt = paste("printf(stream,",'"~a",',str,');',sep="")
  sep = "\n newline(stream); \n"
  txt = paste(txt,collapse=sep)
  txt = c(txt,"\n newline(stream); \n")
  
  add(txt)
}

add.code = function(str, code.file = paste(getwd(),'/maxima_code.mac',sep="") ) {
  restore.point("add.code")
  
  str = merge.lines(str,";")
  str = str.trim(sep.lines(str,";"))
  
  str = str[str!=""]
  
  
  write.text(paste(str,";"),code.file)
  
  # Loop through every line of code
  if (length(str)==0)	return();
  
  
  for (i in 1:NROW(str)) {
    code = str[i]
    
    assign.pos = str.find(code,":",simplify=TRUE)
    if (!has.substr(code,":")) {
      add.comment(code)
      add.tex(code)
    } else {
      # An assigmnet like x: 5*t^2
      add(paste(code,";",sep=""))
      add.comment(code)
      left = str.split(code,":",first=TRUE)[1]
      add.tex(left)
    }
  }
}

add.simplify = function(term,lyma=lyma) {
  restore.point("add.simplify")
  txt =str.replace(lyma$simp.pat,"[TERM]",term)	
  if (txt=="") {
    txt = term
  }
  txt = paste("printf(stream,",'"~a", tex(',txt,',false));',sep="")
  sep = "\n newline(stream); \n"
  txt = paste(txt,collapse=sep)
  txt = c(txt,"\n newline(stream); \n")
  
  add(txt)
  
  #sep.lines(txt)
  #sep.lines(txt.out)
}

add.comment=function(txt, layout="Standard") {
  sep = "\n newline(stream); \n"
  txt = paste('printf(stream,"~a","', txt,'");' , collapse=sep)
  
  layout.txt = paste('printf(stream,"~a","', '##Layout:', layout,'");', sep="")
  txt.out <<-c(txt.out,layout.txt,sep,txt,sep)
  
}

add.tex=function(txt) {
  sep = "\n newline(stream); \n"
  txt = paste("\n printf(stream,",'"~a", tex(',txt,",false));",sep="")
  txt = paste(txt,collapse=sep)
  txt.out <<-c(txt.out,txt,sep)
}

add = function(txt,sep="") {
  txt.out<<-c(txt.out,txt)
}

single.sign = function(txt=lyma$txt,lyma=new.lyma(txt)) {
  restore.point("single.sign")
  #rerestore.point("single.sign")
  
  kill.all()
  txt.out <<-NULL
  label.out <<-NULL
  
  add.lyma.code(lyma)
  
  txt = merge.lines(txt)
  txt = do.subst(txt,lyma)
  
  add.sign(txt,lyma=lyma)
  ret.max = call.maxima(txt.out,lyma=lyma)
  
  ret = read.maxima.out(ret.max$out.file)[1]
  sign.names = list(pnz="sign unknown",pn="not 0",pz="positive or zero",pos="strictly positive",neg="strictly negative",nz="negative or zero")
  if (ret %in% names(sign.names))
    ret = sign.names[[ret]]
  
  return(ret)
}



single.diff = function(txt=lyma$txt,lyma=new.lyma(txt)) {
  restore.point("single.diff")
  #rerestore.point("single.diff")
  
  kill.all()
  
  txt.out <<-NULL
  label.out <<-NULL
  
  txt = merge.lines(txt)
  txt = do.subst(txt,lyma)
  
  #txt = convert.math(txt,lyma=lyma)
  #add.tex(txt)
  
  pat = lyma$simp.pat
  pat = convert.patterns(pat)
  
  add.diff(txt,var=lyma$var,lyma=lyma)
  
  ret.max = call.maxima(txt.out,lyma=lyma)
  ret = convert.maxima.output(ret.max$out.file)
  txt = ret$txt[ret$math.rows]
  return(txt)
}

single.solve = function(txt=lyma$txt,lyma=new.lyma(txt)) {
  restore.point("single.simplify")
  #rerestore.point("single.simplify")
  
  kill.all()
  
  txt.out <<-NULL
  label.out <<-NULL
  
  txt = merge.lines(txt)
  txt = do.subst(txt,lyma)
  
  #txt = convert.math(txt,lyma=lyma)
  #add.tex(txt)
  
  pat = lyma$simp.pat
  pat = convert.patterns(pat)
  
  add.solve(txt,var=lyma$var,lyma=lyma,make.lyx.file=FALSE)
  
  ret.max = call.maxima(txt.out,lyma=lyma)
  ret = convert.maxima.output(ret.max$out.file)
  txt = ret$txt[ret$math.rows]
  return(txt)
}

single.simplify = function(txt=lyma$txt,lyma=new.lyma(txt)) {
  restore.point("single.simplify")
  #rerestore.point("single.simplify")
  
  kill.all()
  
  txt.out <<-NULL
  label.out <<-NULL
  
  
  #var = find.all.var(txt)$var
  #var = convert.math(var,lyma=lyma)
  #simp.var = var
  #simp.pattern=read.text("maxima_pattern.txt")
  txt = merge.lines(txt)
  txt = do.subst(txt,lyma)
  
  txt = convert.math(txt,lyma=lyma)
  #add.tex(txt)
  
  add.simplify(txt,lyma=lyma)
  
  ret.max = call.maxima(txt.out,lyma=lyma)
  ret = convert.maxima.output(ret.max$out.file)
  txt = ret$txt[ret$math.rows]
  
  txt = txt[1]
  return(txt)
}

read.lyx.math = function(txt=lyma$txt,lyma = new.lyma(txt),file=NULL) {
  restore.point("read.lyx.math")
  #rerestore.point("read.lyx.math")
  
  txt.out <<-NULL
  label.out <<-NULL
  
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
    return(single.simplify(txt,lyma));
  }
  
  
  
  crows.slash = which(str.left(txt,2)=="\\#")
  txt[crows.slash] = str.remove.ends(txt[crows.slash],1,0)
  
  com = txt[crows]
  com.start = crows
  com.end = c(crows[-1],NROW(txt)+1)-1
  
  com.i = 1
  count.subst = 0
  while(com.i <= NROW(com)){
    rows = com.start[com.i]:com.end[com.i]		
    
    str = txt[rows]
    
    # Only keep only the part after command in first row
    space.pos = str.find(str," ",first=TRUE,simplify=TRUE)
    if (length(space.pos)>0) {
      str[1] = str.substr(str[1],space.pos+1,nchar(str[1]))
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
    if (str.left(com[com.i],6)=="#SUBST") {
      #browser()
      mat = eq.to.leftright.mat(str)
      if (length(mat)>0) {
        txt <- str.replace.list(txt,mat[,2],mat[,1])
      }
      if (count.subst>0) {
        lyma$subst = rbind(lyma$subst,mat)
      } else {
        lyma$subst = mat
      }
      count.subst = count.subst+1
      lyma.to.gui(lyma)
    }	
    else if (str.left(com[com.i],13)=="#SIMP.PATTERN") {
      #stop()
      simp.patterns <- convert.patterns(str)
      lyma$simp.pat <- simp.patterns
      lyma.to.gui(lyma)
    }
    else if (str.left(com[com.i],7)=="#ASSUME") {
      #stop()
      lyma$assum <- convert.assumptions(str)
      lyma.to.gui(lyma)
    }
    else if (str.left(com[com.i],4)=="#FUN") {
      #stop()
      str = str[str!=""]
      str = convert.math(str,final.curley = "remove",lyma=lyma)
      lyma$fun <- str
      lyma.to.gui(lyma)
    } 		 		
    else if (str.left(com[com.i],7)=="#MAXIMA") {
      #str = txt[rows]
      lyma.to.gui(lyma)
      old.str = str
      str = convert.code(str,lyma)
      add.code(str)
      call.maxima.and.lyx(txt.out,lyma=lyma)
    }	else if (str.left(com[com.i],2)=="#R") {
      #str = txt[rows]
      lyma.to.gui(lyma)
      str = convert.code(str,lyma)
      return(sep.lines(str))
    }	else {
      mywarning(paste("Unknown command", com[com.i], ". Commands must be in CAPITAL letters"))
    }
    com.i=com.i+1
  }
  
  #write.text(txt.out,"outtest.txt")
  return(NULL)
}

