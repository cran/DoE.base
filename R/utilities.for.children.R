### Authors of these functions: Boyko Amarov and Ulrike Groemping

### auxiliary functions
    oa2symb <- function(name){ 
        ## converts oa 2 symbolic
        paste(apply(matrix(unlist(strsplit(name,".",fixed=TRUE))[-1],
                byrow=TRUE,ncol=2),1,paste,collapse="~"),collapse=";")
        }

    symb2oa <- function(nbRuns, descr) {
        ## Converts the symbolic description of an array to the name used
        ## in the DoE.base package
        descr <- sub(";$", "", descr, perl = TRUE)
        descr <- gsub(";|~", ".", descr, perl = TRUE)
        paste("L", paste(c(nbRuns, descr), collapse = "."), sep = "")
      }

parseArrayLine <- function (array.line)
{
    array.descr <- vector("list", 2)
    names(array.descr) <- c("descr", "lineage")
    array.descr$descr <- c(nbRuns = array.line$nruns,
        descr = oa2symb(array.line$name))
    array.descr$lineage <- vector("list", 2)
    names(array.descr$lineage) <- c("parent", "repl.rules")
    tmp.lineage <- array.line$lineage
    if (!(is.na(tmp.lineage) || tmp.lineage == "")) {
        tmp <- strsplit(as.character(tmp.lineage), ":")
        array.descr$lineage$parent$descr <- tmp[[1]][1]
        array.descr$lineage$parent <- c(nbRuns = array.line$nruns, descr = tmp[[1]][1])
        rules <- strsplit(gsub("(","",tmp[[1]][-1], fixed=TRUE), ")")
        rules <- unlist(rules)
        array.descr$lineage$repl.rules <- lapply(rules, function(x) {
            split.rule <- unlist(strsplit(x, "!"))
            c(target = split.rule[1], nbRuns = as.numeric(unlist(strsplit(split.rule[1],"~",fixed=TRUE))[1]), 
               repl = split.rule[2])
        })
    }
    else {
        cat("No lineage information found.\n")
        array.descr$lineage <- NULL
    }
    return(array.descr)
}

getArray <- function (nbRuns, descr) 
{
    ## function to retrieve an array 
    
    ## either retrieve existing array,
    ## or create full factorial (ordered from slow to fast changing)
    
    ## could this be simplified ?
    if (length(descr) == 1) {
        descr <- unlist(strsplit(descr, ";"))
    }
    tmp <- strsplit(descr, "~")
    tmp.ord <- sapply(tmp, "[[", 1)
    descr <- descr[order(as.numeric(tmp.ord))]
    if (prod(sapply(tmp,function(obj) as.numeric(obj[1])^as.numeric(obj[2])))==nbRuns){ 
        hilf <- as.matrix(expand.grid(sapply(tmp,
                     function(obj) rep(list(1:obj[1]),obj[2]))))
        hilf <- hilf[ord(hilf),]
    if (ncol(hilf) <= 50) colnames(hilf) <- Letters[1:ncol(hilf)]
             else colnames(hilf) <- paste("F",1:ncol(hilf),sep="")
       rownames(hilf) <- 1:nbRuns
       attr(hilf,"origin") <- "full factorial"
       attr(hilf,"class") <- c("oa","matrix")
       return(hilf)
        }
    tmp <- paste(descr, collapse = ".")
    tmp <- gsub("~", ".", tmp, perl = TRUE)
    DoE.oa.name <- paste(paste("L", nbRuns, sep = ""), tmp, sep = ".")
    ## changed for making importing of DoE.base possible; 
    ## relies on exporting of arrays by importing packages
    return(get(DoE.oa.name))
}

genChild <- function (array.list)
{
  ## array.list is an output from parseArrayLine
  
  hilffun <- function(name) matrix(as.numeric(unlist(strsplit(name,".",fixed=TRUE))[-1]),byrow=2,ncol=2)
  ## creates table from name
  ## first col: nlevels
  ## second col: number of occurrences in target
  
  targetname <- symb2oa(array.list$descr[1],array.list$descr[2])
  targetmat <- hilffun(targetname)

  oacatrow <- which(oacat$name==targetname)
  if (length(oacatrow)==0) {
             oacat3row <- which(oacat3$name==targetname)
             from <- "oacat3"
  }
  else from <- "oacat"

    ## getting and describing parent array
    parent.array <- getArray(array.list$lineage$parent[1],
                             array.list$lineage$parent[2])
    curarray <- parent.array
    curname <- symb2oa(array.list$lineage$parent[1],array.list$lineage$parent[2])
    curmat <- hilffun(curname)
    ## July 2021: enable **two** different expansions of same number of levels 
    ##            (needed for 8-level)
    tab <- table(sapply(array.list$lineage$repl.rules, function(obj) obj["nbRuns"]))
    dupls <- any(tab>1) 
    if (dupls) {
       duplevels <- as.numeric(names(tab)[which(tab>1)])
       ## only the duplicated replacement
       ## implemented for a single duplicate only
       if (length(tab)==1){
          ## currently no other cases occur
          replevmat1 <- hilffun(symb2oa(duplevels, array.list$lineage$repl.rules[[1]][3])) 
          replevmat2 <- hilffun(symb2oa(duplevels, array.list$lineage$repl.rules[[2]][3])) 
          # curmat holds same thing for the parent
         ## equation for obtaining the frequency needed for each rule
         nam2ohne1 <- setdiff(replevmat2[,1], replevmat1[,1])[1] ## first unique
         mat <- rbind(c(1,1),  ## for b1=total number of duplevels that are expanded
                      c(0,replevmat2[which(replevmat2[,1]==nam2ohne1),2]))  
                           ## for b2=no of nam2ohne1 in target minus parent
         bvec <- c( ifelse(any(targetmat[,1] == duplevels), 
                           curmat[which(curmat[,1]==duplevels),2] -
                              targetmat[which(targetmat[,1]==duplevels),2],
                           curmat[which(curmat[,1]==duplevels),2]),
                    ifelse(any(curmat[,1] == nam2ohne1), 
                           targetmat[which(targetmat[,1]==nam2ohne1),2] -
                              curmat[which(curmat[,1]==nam2ohne1),2],
                           targetmat[which(targetmat[,1]==nam2ohne1),2])
                    )
         repfreq <- solve(mat, bvec)
         array.list$lineage$repl.rules[[1]] <- 
              c(array.list$lineage$repl.rules[[1]], repfreq= as.character(repfreq[1]))
         array.list$lineage$repl.rules[[2]] <- 
              c(array.list$lineage$repl.rules[[2]], as.character(repfreq[2]))
         rm(repfreq)
       }
       }
       ## end of July 2021 preprocessing change

    ### looping through replacement rules
    for (i in 1:length(array.list$lineage$repl.rules)){
      replacement <- getArray(array.list$lineage$repl.rules[[i]][2],
         array.list$lineage$repl.rules[[i]][3])
      nbLevels.target <- nrow(replacement)
      repname <- symb2oa(array.list$lineage$repl.rules[[i]][2],
                    array.list$lineage$repl.rules[[i]][3])
      repmat <- hilffun(repname)
      ### how often is this replacement needed ?
      if (dupls) repfreq <- array.list$lineage$repl.rules[[i]][4] else{
      if (any(targetmat[,1] == nbLevels.target))
         repfreq <- curmat[which(curmat[,1] == nbLevels.target), 2] -
                 targetmat[which(targetmat[,1] == nbLevels.target), 2]
      else repfreq <- curmat[which(curmat[,1] == nbLevels.target), 2]
      }
      for (j in repfreq:1){
          ## do the replacements as many times as needed
          fct.levels.parent <- apply(curarray, 2, function(x) length(unique(x)))
          which.target.col <- which(fct.levels.parent == nbLevels.target)[j]
          curarray <- cbind(curarray[,-which.target.col],
                 replacement[curarray[,which.target.col],])
      ## columns 12 and 13 are swapped vs SAS in L24.2.14.6.1 with 15 columns
      ## columns 14 and 15 are swapped vs SAS in L24.2.16.3.1 with 17 columns
      ## the code below tried to change this, but didnt succeed
       #   if (which.target.col > 1 & which.target.col < ncol(curarray))
       #   curarray <- cbind(curarray[,1:(which.target.col-1)],
       #          replacement[curarray[,which.target.col],],curarray[,(which.target.col+1):ncol(curarray)])
       #   else {if (which.target.col == ncol(curarray)) 
       #        curarray <- cbind(curarray[,-which.target.col],
       #          replacement[curarray[,which.target.col],])
       #          else curarray <- cbind(replacement[curarray[,which.target.col],],
       #             curarray[,-which.target.col])
       #          }
      }
      fct.levels.parent <- apply(curarray, 2, function(x) length(unique(x)))
      curarray <- curarray[,sort(fct.levels.parent, index.return=TRUE)$ix]
      curmat <- table(fct.levels.parent)
      curmat <- cbind(as.numeric(names(curmat)),curmat)
      curname <- paste("L",paste(c(nrow(curarray),t(curmat)),collapse="."),sep="")
    }
    if (ncol(curarray) <= 50) colnames(curarray) <- Letters[1:ncol(curarray)]
      else colnames(curarray) <- paste("F",1:ncol(curarray), sep=".")
    curarray <- curarray[ord(curarray),]
    class(curarray) <- c("oa","matrix")
    attr(curarray, "origin") <- c(ifelse(from=="oacat", "Kuhfeld collection",
                                         "oacat3"), get(from)[ifelse(from=="oacat",oacatrow,oacat3row),]$lineage)
    return(curarray)
}