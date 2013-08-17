
#' Convert maxima 'tex' output to Latex expression readable by Lyx
convert.maxima.output = function(out.file= MAXIMA.OUTFILE,change.over = c("def","frac","inv","")) {  
  restore.point("convert.maxima.output")
  
  # Read the generated file and adapt the Maxima Tex output, which unfortunately is quite ugly
  
  # Perhaps we have to make sure here that Maxima has finished its operation... let's hope it works
  txt = read.maxima.out(out.file)
  
  txt = merge.lines(txt,"\n")
  txt = sep.lines(txt,"?")
  txt = str.trim(txt)
  txt = txt[txt!=""]
  
  math.rows = str.left(txt,3)=="#S#"
  org.txt = txt
  txt = txt[math.rows]
  txt = str.remove.ends(txt,3,0)
  txt[txt=="\\mathbf{true}"]="###"
  
  txt = maxima.tex.to.lyx(txt,change.over=change.over)
  
  org.txt[math.rows]=txt
  txt = org.txt
  
  delete.rows = txt=="###"
  txt = txt[!delete.rows]
  math.rows = math.rows[!delete.rows]
  
  math.rows = which(math.rows)
  return(list(txt=txt,math.rows = math.rows))
  
}


maxima.tex.to.lyx = function(txt,change.over=c("def","frac","inv","")) {
  restore.point("maxima.tex.to.lyx")
  #rerestore.point("maxima.tex.to.lyx")
  
  txt = str.replace(txt,"\\left[","")
  txt = str.replace(txt,"\\right]","")
  
  txt = str.replace(txt,"\\,"," ")
  txt = str.replace(txt,"\\it \\%"," \\")
  txt = str.replace(txt,"\\it"," ")
  #txt = str.replace(txt,"$","")
  # Make subscripts nice again
  txt.back = txt
  txt = txt.back
  # I don't know why theese strange strings appear
  txt = str.replace(txt,"\\_","_")
  txt = str.replace(txt,"__","_")
  # Make nice curley for subscript
  txt = str.replace(txt,'_([a-zA-Z0-9]+)','_{\\1}',fixed=FALSE)
  # Remove curleys around variables with subscripts
  txt = str.replace(txt,'\\{[ \t]*([a-zA-Z0-9]+_\\{[a-zA-Z0-9]+\\})[ \t]*\\}',' \\1 ',fixed=FALSE)
  # Remove curleys around varepsilon
  txt = str.replace(txt,'\\{[ \t]*(varepsilon)[ \t]*\\}',' \\1 ',fixed=FALSE)
  
  str = merge.lines(txt,"?")
  
  change.over = change.over[1]
  if (change.over != "") {
    # Transform {{x}\over{a+b}} into frac{x}{a+b}
    while (TRUE) {
      over.pos = str.find(str,"\\over")
      if (length(over.pos)==0) {
        break;
      }
      
      ret = make_simple_block_info(str=str,"{","}")[[1]]
      curley.pos = cbind(ret$startpos,ret$endpos)
      ret = combine.pos.list.and(list(curley.pos,over.pos,curley.pos))
      pos = ret$pos;  ind = ret$ind
      depth = get.pos.depth(pos)
      pos = pos[depth==0,,drop=FALSE];ind = ind[depth==0,,drop=FALSE];
      if (length(pos)==0) {
        stop("\\over found but not two adjacent curleys!")
      }
      curley.str =  str.at.pos(str,curley.pos)
      
      # Want to estimate the length of a fraction
      # If to long write as inv
      
      #restore.point("convert.maxima.output_1")
      #rerestore.point("convert.maxima.output_1")
      #stop()
      
      if (change.over == "def") {
        length.str = c(curley.str[ind[,3]],curley.str[ind[,1]])
        length.str = str.replace(length.str,'\\\\frac{','',fixed = TRUE)					
        length.str = str.replace(length.str,'_','',fixed = TRUE)					
        length.str = str.replace(length.str,'^','',fixed = TRUE)					
        length.str = str.replace(length.str,'\\\\[a-zA-Z0-9]*','?',fixed = FALSE)		
        
        str.len = matrix(nchar(length.str),ncol=2,byrow=FALSE)
        # Maximum of numerator and denominator characters
        str.len = pmax(str.len[,1],str.len[,2])
        max.len  = 150
        as.inv = str.len > max.len
      } else if (change.over == "frac") {
        as.inv = rep(FALSE,NROW(pos))
      } else {
        as.inv = rep(FALSE,NROW(pos))
      }
      
      
      # Add adjacent { } around the over term
      pos[,1] = pos[,1]-1
      pos[,2] = pos[,2]+1
      
      new.str = rep("", NROW(pos))
      
      new.str[!as.inv] = paste("\\frac",curley.str[ind[!as.inv,1]],curley.str[ind[!as.inv,3]],sep="")
      curley.str = str.remove.ends(curley.str,1,1)
      new.str[as.inv] = paste("(",curley.str[ind[as.inv,3]],")^{-1} (",curley.str[ind[as.inv,1]],")",sep="")
      
      str = str.replace.at.pos(str,pos,new.str)	
    }
  }
  
  txt = sep.lines(str,"?")
  
  #Make small brackets
  txt = str.replace(txt,"\\left","")
  txt = str.replace(txt,"\\right","")
  
  
  # Greeks that had a subscript have no \ . Fix that
  txt = str.replace(txt,greek.or,'\\\\\\1',fixed=FALSE)	
  #txt = str.replace.list(txt,greek,paste("\\",greek," ",sep=""))
  txt = str.replace(txt,"\\\\","\\")
  txt = str.replace(txt," _","_")
  
  return(txt)
}


find.all.var = function(txt,form=c("latex.normal","maxima")) {
  restore.point("find.all.var")
  #rerestore.point("find.all.var")
  
  form = form[1]
  stopifnot(form=="latex.normal")
  
  str = txt
  if (form[1] == "latex.normal") {
    # Find single character latin variables and put them into a curley
    subscript.pos = str.find(str,'[0-9a-zA-Z]+_\\{[0-9a-zA-Z]+\\}',fixed=FALSE)  
    var.subscript = unique(str.at.pos(str,subscript.pos))
    
    slashword.pos = str.find(str,'\\\\([0-9a-zA-Z])+',fixed=FALSE)
    no.var.pos = str.inpos(str,rbind(slashword.pos,subscript.pos))
    
    pos = str.find(str,'([a-z])',ignore=no.var.pos,fixed=FALSE)
    var.single = unique(str.at.pos(str,pos))
    
    # Find all greeks (don't worry whether they are in a subscript or not)
    pos = str.find(str,greek.or,fixed = FALSE,ignore.pos = subscript.pos)
    if (length(pos)>0) {
      var.greek = paste("\\",unique(str.at.pos(str,pos)),sep="")
    } else {
      var.greek = character(0)
    }
  } else if (form[1] == "maxima") {
    # Find single character latin variables and put them into a curley
    subscript.pos = str.find(str,'[a-zA-Z]+[0-9a-zA-Z]*_[0-9a-zA-Z]+',fixed=FALSE)	
    var.subscript = unique(str.at.pos(str,subscript.pos))
    
    slashword.pos = str.find(str,'\\\\([0-9a-zA-Z])+',fixed=FALSE)
    no.var.pos = str.inpos(str,rbind(slashword.pos,subscript.pos))
    
    pos = str.find(str,'([a-z])','{\\1}',fixed=FALSE,ignore=no.var.pos)
    var.single = unique(str.at.pos(str,pos))
    
    # Find all greeks (don't worry whether they are in a subscript or not)
    pos = str.find(str,greek.or,fixed = FALSE,ignore.pos=subscript.pos)
    var.greek = unique(str.at.pos(str,pos))
  }
  return(list(var=c(var.single,var.greek,var.subscript)))
}


convert.code = function(str,lyma=new.lyma()) {
  restore.point("convert.code")
  #rerestore.point("convert.code")
  
  str = merge.lines(str,"\n")
  ret = make_simple_block_info(str=str,"$","$")[[1]]
  math.pos = cbind(ret$startpos,ret$endpos)
  math.txt = str.at.pos(str,math.pos)
  if (length(math.txt)>0) {
    math.txt = convert.math(math.txt,lyma=lyma)
    str = str.replace.at.pos(str,math.pos,math.txt)
  }
  str = sep.lines(str,"\n")
  
  comment.rows = (str.left(str,3)=="\\#'" | str.left(str,2)=="#'")
  str[comment.rows] = paste("/*",str.remove.ends(str[comment.rows],2,0),"*/")
  str
}



convert.var = function(var,lyma=new.lyma()) {
  # Converts a single variable or a list of variables into maxima format
  restore.point("convert.var")
  #rerestore.point("convert.var")
  if (nchar(var)==0)
    return(var)
  
  var <- str.replace.list(var,lyma$subst[,2],lyma$subst[,1])
  var = convert.math(var,final.curley="remove",lyma=lyma)
  var
  
}


eq.to.leftright.mat = function(str,colnames=NULL) {
  restore.point("eq.to.leftright.mat")
  #rerestore.point("eq.to.leftright.mat")
  
  #stop()
  EMPTY.MAT = matrix(NA,nrow=0,ncol=2)
  
  str = str.replace(str,"\n",";")
  str = merge.lines(str,";")
  str = sep.lines(str,";");
  
  # Only consider rows where assignment takes place
  row.eq = has.substr(str,"=")
  str = str[row.eq]
  
  if (length(str)==0 | identical(str,""))
    return(EMPTY.MAT)
  
  # Remove some math trash
  str = str.replace(str,"$","")  		
  
  center = str.find(str,"=",first=TRUE,simplify=TRUE)
  mat = cbind(str.trim(str.substr(str,1,center-1)),str.trim(str.substr(str,center+1,nchar(str))))
  if (!is.null(colnames))
    colnames(mat)=colnames
  return(mat)	
}

convert.assumptions = function(txt) {
  restore.point("convert.assumptions")
  #rerestore.point("convert.assumptions")
  
  txt = merge.lines(txt,";")
  txt = str.replace(txt,"\\leq","<=")
  txt = str.replace(txt,"\\geq",">=")
  txt = convert.math(txt)
  txt = str.replace(txt," ","")
  
  txt = sep.lines(txt,";")
  txt = str.replace(txt,"()","")
  txt = str.replace(txt,"%","")
  txt = txt[txt!=""]
  
  comp  = str.find(txt,'[><=]+',fixed=FALSE,simplify=FALSE,matches=TRUE)
  terms = str.split(txt,'[><=]+',fixed=FALSE)
  
  # Split inequality chains 0 < a < x into 0 < a; a < x 
  for (i in 1:NROW(comp)) {
    if (length(comp[[i]])>1) {
      temp.txt = ""
      for (j in 1:length(comp[[i]])) {
        temp.txt = paste(temp.txt,terms[[i]][j],comp[[i]][j],terms[[i]][j+1],";",sep="")
      }
      txt[i] = temp.txt
    }
  }
  txt = sep.lines(txt,";")
  txt = txt[txt!=""]
  txt
  
  # Transform lists 0 < x,y,z into 0<x,0<y,0<z
  rows = which(has.substr(txt,","))
  if (length(rows)>0) {
    comp = str.find(txt,'[><=]+',fixed=FALSE,simplify=!TRUE,matches=TRUE)
    for (i in rows) {
      
      acomp = comp[[i]][1,1]
      str = txt[i]
      ret = str.split(str,acomp,simplify=TRUE)
      left = str.split(ret[1],",",simplify=TRUE); 
      right = str.split(ret[2],",",simplify=TRUE);
      gm = make.grid.matrix(list(left,right))
      str = paste(gm[,1],acomp,gm[,2],sep="",collapse=",")
      txt[i] = str
    }
  }
  txt = merge.lines(txt,",")
  return(txt)
}

convert.patterns = function(str) {
  restore.point("convert.patterns")
  #rerestore.point("convert.patterns")
  
  str = sep.lines(str)
  str = merge.lines(str,collapse=";")
  str = str.replace(str,"%;",";")
  
  # #Replace mathematical expressions to Maxima format
  # ret = make_simple_block_info(str,"$","$")[[1]]
  # mat.pos = cbind(ret$startpos,ret$endpos)
  # if (length(mat.pos)>0) {
  # lat.mat = str.at.pos(str,mat.pos)
  # max.mat = convert.math(lat.mat)
  # str = str.replace.at.pos(str,mat.pos,max.mat)
  # }
  #str = str.replace(str,"$","")
  
  # A pattern in each row (or some junk rows)
  str = str.tokenize(str,";")
  str = str[str!="{}"]
  empty.rows = which(str=="")
  if (length(empty.rows)>1) {
    str = str[-empty.rows[-1]]
  }
  
  # Need to augment short form of patterns
  str = sep.lines(str)
  rows = which(str.left(str,2)=="\\%")
  if (length(rows)>0) {
    str[rows] = paste("format([TERM],",str[rows],")",sep="")
  }
  # Need to augment short form of patterns
  rows = which(str.left(str,1)=="%")
  if (length(rows)>0) {
    str[rows] = paste("format([TERM],\\",str[rows],")",sep="")
  }
  
  
  # Add [TERM] where missing
  str = str.replace(str,"()","([TERM])")
  
  str = str.replace(str,"{[}","[")		
  str = str.replace(str,"{]}","]")
  
  # Add postrans
  str[str!=""] = paste("postrans(\\",str[str!=""],")",sep="")
  str[str==""] = "[TERM]"	
  
  return(str)
}


do.subst = function(str,lyma) {
  restore.point("do.subst")
  mat = lyma$subst
  if (length(mat)>0) {
    str <- str.replace.list(str,mat[,2],mat[,1])
  }
  str
}
# Reverses the substitution of variable names in tex Code
# used to adapt the result of Maxima to original LYX formulas
do.reverse.subst = function(txt,lyma) {
  if (NROW(lyma$subst)>0 & nchar(txt)>0) { 
    txt = str.replace.list(txt, lyma$subst[,1],lyma$subst[,2])
  }
  txt
}


# Return
convert.math = function(str,final.curley=c("round","remove","keep"),lyma=new.lyma()) {
  
  restore.point("convert.math")
  #rerestore.point("convert.math")
  res = list()
  # Mege all lines together to one string
  str = str.trim(merge.lines(str))
  
  if (str=="") {
    warning("convert.math called with empty string")
    return(str)
  }
  
  
  # Remove trash
  str = str.replace(str,"&","") # Delimeters for equarray
  str = str.replace(str,"\\\\","")
  str = str.replace(str,"\\left","")
  str = str.replace(str,"\\right","")
  #str = str.replace(str," ","")
  str = str.replace(str,"\\[","")
  str = str.replace(str,"\\]","")
  str = str.replace(str,"$","")
  
  
  # Find fractions \frac{numerator}{denominator}
  
  str = str.replace.by.blocks(str,"\\frac{_SUB_}{_SUB_}","(_SUB1_)/(_SUB2_)",
                       block.start = "{", block.end = "}")
  
  #REDO FINDING OF VARIABLES WITHOUT SUBSCRIPT
  
  #Remove curleys from substr
  str = str.replace(str,'_\\{(\\\\?[0-9a-zA-Z]*)\\}','_\\1 ',fixed=FALSE)
  # Find single character latin variables and put them into a curley
  slashword.pos = str.find(str,'\\\\([0-9a-zA-Z])+',fixed=FALSE)
  if (!is.matrix(slashword.pos)) {
    stop("Error in convert.math Need to fix str.find")
  }
  
  # Need to check which of the two following lines is correct....
  #subscript.pos = str.find(str,'[a-zA-Z]+[0-9a-zA-Z]*_[0-9a-zA-Z]+',fixed=FALSE)	
  subscript.pos = str.find(str,'[a-zA-Z]_[0-9a-zA-Z]+',fixed=FALSE)	
  
  no.var.pos = str.inpos(str,rbind(slashword.pos,subscript.pos))
  rbind(str,
        paste(no.var.pos*1,collapse=""),
        paste(str.inpos(str,slashword.pos)*1,collapse=""),
        paste(str.inpos(str,subscript.pos)*1,collapse=""))
  
  str = str.replace(str,'([a-zA-Z])','{\\1}',fixed=FALSE,ignore=no.var.pos)
  str
  
  
  #Remove \ from greek letters
  greek = glob()$greek
  greek.or = glob()$greek.or
  
  str.test = str.replace.list(str,paste("\\",greek,sep=""),paste(greek," ",sep=""))
  
  str = str.replace(str,paste('\\\\',greek.or,sep=""),'\\1 ',fixed=FALSE)	
  stopifnot(str.test==str)
  
  str = str.replace(str," _","_")
  
  # Put greeks without subscripts into curley
  subscript.pos = str.find(str,'[a-zA-Z]+[0-9a-zA-Z]*_[0-9a-zA-Z]+',fixed=FALSE)	
  no.var.pos = str.inpos(str,subscript.pos)
  str = str.replace(str,greek.or,'\\{\\1\\}',fixed=FALSE,ignore=no.var.pos)
  str
  
  #Put numbers into curleys
  subscript.pos = str.find(str,'[a-zA-Z]+[0-9a-zA-Z]*_[0-9a-zA-Z]+',fixed=FALSE)		
  str = str.replace(str,'([0-9]+\\.?[0-9]*)','\\{\\1\\}',fixed=FALSE,ignore.pos=subscript.pos)
  
  #Put variables with subscripts into curleys
  varsub = '([0-9a-zA-Z]*_[0-9a-zA-Z]*)'
  str = str.replace(str,varsub,"{\\1}",fixed=FALSE)
  str
  
  #Convert edgy brackets to round ones (otherwise Maxima does not work correctly)
  str = str.replace(str,"[","(",fixed=TRUE)
  str = str.replace(str,"]",")",fixed=TRUE)
  
  #Put curleys around round brackets
  str = str.replace(str,"(","{(",fixed=TRUE)
  str = str.replace(str,")",")}",fixed=TRUE)
  
  # Remove spaces
  str = str.replace(str," ","") 
  
  # Add * between terms
  str = str.replace(str,"}{","}*{",fixed=TRUE)
  
  # Remove curleys
  #str = str.replace(str,"{","",fixed=TRUE)
  #str = str.replace(str,"}","",fixed=TRUE)
  # Replace curleys with round
  if (final.curley[1]=="remove") {
    str = str.replace(str,"{","",fixed=TRUE)
    str = str.replace(str,"}","",fixed=TRUE)
  } else if (final.curley[1] == "round") {
    str = str.replace(str,"{","(",fixed=TRUE)
    str = str.replace(str,"}",")",fixed=TRUE)
  }
  
  # Replace (f)*(...) with f(...) if f is a function
  if (length(lyma$fun)>0 & !identical(lyma$fun,"")) {
    restore.point("convert.math_fun")
    #rerestore.point("convert.math_fun")
    
    fun.or = paste("(",str.list.to.regexp.or(lyma$fun),")",sep="")
    pat = paste('\\Q(\\E',fun.or,'\\Q)*((\\E',sep="")
    pos= str.find(str,pat,fixed=FALSE)
    str.at.pos(str,pos)
    
    str = str.replace(str,pat,'\\1((',fixed=FALSE)
  }
  # Remove some excessive braces
  var.pat = "([0-9a-zA-Z_]*)"
  for (i in 1:4) {
    str = str.replace(str,paste0("([\\(\\+-\\*/\\^])\\(",var.pat,"\\)([\\)\\+-\\*/\\^$])"),'\\1\\2\\3',fixed=FALSE)
    str = str.replace(str,paste0("\\(\\(","([0-9a-zA-Z_\\+-\\*/\\^ ]*)","\\)\\)"),
                                '(\\1)',fixed=FALSE)    
  }
  str = str.replace(str,paste0("^\\(",var.pat,"\\)"),'\\1',fixed=FALSE)
  str = str.replace(str,paste0("\\(",var.pat,"\\)$"),'\\1',fixed=FALSE)
  
  # Separate lines again
  str = sep.lines(str)
  str	
}
