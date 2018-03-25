#symbolicDA2RSDA<-function(symbolic){
#token=paste("temp",sample(1:10000000,1),sep="_")
#save.SO(symbolic,token)
#t<-SODAS.to.RSDA(token)
#file.remove(token)
#t
#}



# INCLUDED FROM RSDA, IT WILL BE REMOVED WHEN RSDA APPEARS AGAIN ON CRAN  --- START

#' Read a Symbolic Table
#' @name read.sym.table
#' @aliases read.sym.table
#' @author Oldemar Rodriguez Rojas
#' @description It reads a symbolic data table from a CSV file.
#' @usage read.sym.table(file, header = TRUE, sep, dec, row.names = NULL)
#' @param file The name of the CSV file.
#' @param header As in R function read.table
#' @param sep As in R function read.table
#' @param dec As in R function read.table
#' @param row.names As in R function read.table
#' @details
#' The labels $C means that follows a continuous variable, $I means an interval variable, $H
#' means a histogram variables and $S means set variable. In the first row each labels should
#' be follow of a name to variable and to the case of histogram a set variables types the names
#' of the modalities (categories) . In data rows for continuous variables we have just one
#' value, for interval variables we have the minimum and the maximum of the interval,
#' for histogram variables we have the number of modalities and then the probability
#' of each modality and for set variables we have the cardinality of the set and next
#' the elements of the set.
#'
#' The format is the CSV file should be like:
#'
#'   $C   F1 $I F2 F2 $H F3  M1  M2  M3 $S F4 E1 E2 E3 E4 \cr
#'
#' Case1 $C  2.8 $I  1  2 $H  3 0.1 0.7 0.2 $S  4  e  g  k  i\cr
#'
#' Case2 $C  1.4 $I  3  9 $H  3 0.6 0.3 0.1 $S  4  a  b  c  d\cr
#'
#' Case3 $C  3.2 $I -1  4 $H  3 0.2 0.2 0.6 $S  4  2  1  b  c\cr
#'
#' Case4 $C -2.1 $I  0  2 $H  3 0.9 0.0 0.1 $S  4  3  4  c  a\cr
#'
#' Case5 $C -3.0 $I -4 -2 $H  3 0.6 0.0 0.4 $S  4  e  i  g  k\cr
#'
#' The internal format is:\cr

#'   $N\cr

#' [1] 5\cr

#' $M\cr

#' [1] 4\cr

#' $sym.obj.names\cr

#' [1] 'Case1' 'Case2' 'Case3' 'Case4' 'Case5'\cr

#' $sym.var.names\cr

#' [1] 'F1' 'F2' 'F3' 'F4'\cr

#' $sym.var.types\cr

#' [1] '$C' '$I' '$H' '$S'\cr

#' $sym.var.length\cr

#' [1] 1 2 3 4\cr

#' $sym.var.starts\cr

#' [1]  2  4  8 13\cr

#' $meta\cr

#' $C   F1 $I F2 F2 $H F3  M1  M2  M3 $S F4 E1 E2 E3 E4\cr

#' Case1 $C  2.8 $I  1  2 $H  3 0.1 0.7 0.2 $S  4  e  g  k  i\cr

#' Case2 $C  1.4 $I  3  9 $H  3 0.6 0.3 0.1 $S  4  a  b  c  d\cr

#' Case3 $C  3.2 $I -1  4 $H  3 0.2 0.2 0.6 $S  4  2  1  b  c\cr

#' Case4 $C -2.1 $I  0  2 $H  3 0.9 0.0 0.1 $S  4  3  4  c  a\cr

#' Case5 $C -3.0 $I -4 -2 $H  3 0.6 0.0 0.4 $S  4  e  i  g  k\cr

#' $data\cr

#' F1 F2 F2.1  M1  M2  M3 E1 E2 E3 E4\cr

#' Case1  2.8  1    2 0.1 0.7 0.2  e  g  k  i\cr

#' Case2  1.4  3    9 0.6 0.3 0.1  a  b  c  d\cr

#' Case3  3.2 -1    4 0.2 0.2 0.6  2  1  b  c\cr

#' Case4 -2.1  0    2 0.9 0.0 0.1  3  4  c  a\cr

#' Case5 -3.0 -4   -2 0.6 0.0 0.4  e  i  g  k\cr
#'
#' @return Return a symbolic data table structure.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information
#' from complex data. Springer, Germany.
#' @seealso display.sym.table
#' @examples
#' data(example1)
#' write.sym.table(example1, file='temp4.csv', sep='|',dec='.', row.names=TRUE,
#'                 col.names=TRUE)
#' ex1<-read.sym.table('temp4.csv', header=TRUE, sep='|',dec='.', row.names=1)
#' @keywords Symbolic Table
#' @export
#' @importFrom utils read.table
#'
.read.sym.table <- function(file, header = TRUE, sep, dec, row.names = NULL) {
  meta.data <- read.table(file, header, sep = as.character(sep), dec = as.character(dec), 
                          row.names = c(row.names), check.names = FALSE)
  n.sym.objects <- dim(meta.data)[1]
  meta.M <- dim(meta.data)[2]
  sym.var.types <- list()
  sym.var.length <- rep(0, length(meta.M))
  sym.var.names <- list()
  sym.var.starts <- list()
  sym.obj.names <- rownames(meta.data)
  del.columns <- c()
  del.columns.length <- 0
  if (header == TRUE) 
    meta.types <- colnames(meta.data) else stop("Data file have to have a header")
  meta.types.orig <- meta.types
  for (i in 1:length(meta.types)) {
    meta.types[i] <- substr(meta.types[i], start = 1, stop = 2)
  }
  for (j in 1:length(meta.types)) {
    if ((meta.types[j] == "$C") || (meta.types[j] == "$c")) {
      sym.var.types[j] <- "$C"
      sym.var.length[j] <- 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 1
    } else if ((meta.types[j] == "$I") || (meta.types[j] == "$i")) {
      sym.var.types[j] <- "$I"
      sym.var.length[j] <- 2
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 1
    } else if ((meta.types[j] == "$H") || (meta.types[j] == "$h")) {
      sym.var.types[j] <- "$H"
      sym.var.length[j] <- as.integer(meta.data[2, j + 1])
      del.columns[del.columns.length + 1] <- j + 1
      del.columns.length <- del.columns.length + 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 2
    } else if ((meta.types[j] == "$M") || (meta.types[j] == "$m")) {
      sym.var.types[j] <- "$M"
      sym.var.length[j] <- as.integer(meta.data[2, j + 1])
      del.columns[del.columns.length + 1] <- j + 1
      del.columns.length <- del.columns.length + 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 2
    } else if ((meta.types[j] == "$S") || (meta.types[j] == "$s")) {
      sym.var.types[j] <- "$S"
      sym.var.length[j] <- as.integer(meta.data[2, j + 1])
      del.columns[del.columns.length + 1] <- j + 1
      del.columns.length <- del.columns.length + 1
      sym.var.names[j] <- meta.types.orig[j + 1]
      sym.var.starts[j] <- j + 2
    } else sym.var.types[j] <- "NA"
  }
  del1 <- match(sym.var.types, c("$C", "$I", "$H", "$S", "$M"), 0)
  for (k in 1:del.columns.length) {
    sym.var.types[del.columns[k]] <- "$H"
  }
  del2 <- match(sym.var.types, c("$C", "$I", "$H", "$S", "$M"), 0)
  sym.var.types <- sym.var.types[del1 > 0]
  sym.var.length <- sym.var.length[del1 > 0]
  n.sym.var <- length(sym.var.length)
  data.matrix <- as.data.frame(meta.data[, del2 == 0])
  sym.data <- list(N = n.sym.objects, M = n.sym.var, sym.obj.names = sym.obj.names, 
                   sym.var.names = unlist(sym.var.names), sym.var.types = unlist(sym.var.types), 
                   sym.var.length = sym.var.length, sym.var.starts = unlist(sym.var.starts), meta = meta.data, 
                   data = data.matrix)
  
  class(sym.data) <- c("sym.data.table")
  return(sym.data)
}


# INCLUDED FROM RSDA, IT WILL BE REMOVED WHEN RSDA APPEARS AGAIN ON CRAN  --- END


#load("RSDA\\DATA\\Example3.rda",verbose=T)
#rsda.object<-example3
#csv.file<-"example3.csv"
#from.csv<-T
#rsda.object=NULL
#header=T
#row.names=1
#sep=";"
#dec="."


RSDA2SymbolicDA<-function(rsda.object=NULL,from.csv=F,file=NULL, header = TRUE, sep, dec, row.names = NULL) {

if((is.null(rsda.object) || class(rsda.object)!="sym.data.table") && from.csv){
  rsda.object<-.read.sym.table(file=file, header = header, sep=sep, dec=dec, row.names = row.names)
}
individualsNo<-rsda.object$N
indivA<-array("",c(individualsNo,3))
for(i in 1:individualsNo)
{
  indivA[i,1]<-i
  indivA[i,2]<-rsda.object$sym.obj.names[i]
  indivA[i,3]<-i
}
indiv<-as.data.frame(indivA)
names(indiv)<-c("num","name","label")

variablesNo<-rsda.object$M
variablesICNo<-sum(rsda.object$sym.var.types=="$I")
variablesCNo<-sum(rsda.object$sym.var.types=="$C")
variablesNNo<-sum(rsda.object$sym.var.types=="$S")
variablesNMNo<-sum(rsda.object$sym.var.types=="$M")
detailsICA<-array("",c(variablesICNo,4))
detailsCA<-array("",c(variablesCNo,4))
detailsNA<-array("",c(variablesNNo,3))
detailsNMA<-array("",c(variablesNMNo,4))
variablesA<-array("",c(variablesNo,5))
detailsICNo<-0
detailsCNo<-0
detailsNNo<-0
detailsNMNo<-0
detailsListNom<-NULL
detailsListNomModif<-NULL
indivNA<-NULL
indivNMA<-NULL
for(i in 1:variablesNo)
{
  if(rsda.object$sym.var.types[i]=="$C"){
  variablesA[i,1]<-i
  variablesA[i,2]<-rsda.object$sym.var.names[i]
  variablesA[i,3]<-rsda.object$sym.var.names[i]
  variablesA[i,4]<-"C"
  detailsCNo<-detailsCNo+1
  detailsCA[detailsCNo,1]<-0
  detailsCA[detailsCNo,2]<-0
  detailsCA[detailsCNo,3]<-as.numeric(min(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
  detailsCA[detailsCNo,4]<-as.numeric(max(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
  variablesA[i,5]<-detailsCNo
  }

  if(rsda.object$sym.var.types[i]=="$I"){
    variablesA[i,1]<-i
    variablesA[i,2]<-rsda.object$sym.var.names[i]
    variablesA[i,3]<-rsda.object$sym.var.names[i]
    variablesA[i,4]<-"IC"
    detailsICNo<-detailsICNo+1
    detailsICA[detailsICNo,1]<-0
    detailsICA[detailsICNo,2]<-0
    detailsICA[detailsICNo,3]<-as.numeric(min(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
    detailsICA[detailsICNo,4]<-as.numeric(max(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
    variablesA[i,5]<-detailsICNo
  }

  if(rsda.object$sym.var.types[i]=="$S"){
    variablesA[i,1]<-i
    variablesA[i,2]<-rsda.object$sym.var.names[i]
    variablesA[i,3]<-rsda.object$sym.var.names[i]
    variablesA[i,4]<-"N"
    detailsNNo<-detailsNNo+1
    detailsNA[detailsNNo,1]<-0
    detailsNA[detailsNNo,2]<-0
    index<-which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])
    nr<-as.numeric(rsda.object$meta[1,index])
    detailsNA[detailsNNo,3]<-nr
    variablesA[i,5]<-detailsNNo
    for(ii in 1:nr){
      nazwa<-dimnames(rsda.object$meta)[[2]][index+ii]
  #    if(is.null(detailsListNom)){
  #      lp=1
  #    }
  #    else{
  #      lp=nrow(detailsListNom)+1
  #    }
      detailsListNom<-rbind(detailsListNom,c(detailsCNo,ii,nazwa,nazwa))
      for(j in 1:rsda.object$N){
        if(rsda.object$meta[j,index+ii]!=0){
          indivNA<-rbind(indivNA,c(j,i,ii))
        }
      }
    }
    
    
    
  }
  
  if(rsda.object$sym.var.types[i]=="$M"){
    variablesA[i,1]<-i
    variablesA[i,2]<-rsda.object$sym.var.names[i]
    variablesA[i,3]<-rsda.object$sym.var.names[i]
    variablesA[i,4]<-"MN"
    detailsNMNo<-detailsNMNo+1
    detailsNMA[detailsNMNo,1]<-0
    detailsNMA[detailsNMNo,2]<-0
    print("tt")
    
    index<-which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])
    nr<-as.numeric(rsda.object$meta[1,index])
    
    detailsNMA[detailsNMNo,3]<-nr
    variablesA[i,5]<-detailsNMNo
    for(ii in 1:nr){
      nazwa<-dimnames(rsda.object$meta)[[2]][index+ii]
      #    if(is.null(detailsListNom)){
      #      lp=1
      #    }
  
          #    else{
      #      lp=nrow(detailsListNom)+1
      #    }
      detailsListNomModif<-rbind(detailsListNomModif,c(detailsCNo,ii,nazwa,nazwa))
      for(j in 1:rsda.object$N){
          indivNMA<-rbind(indivNMA,c(j,i,ii,rsda.object$meta[j,index+ii]))
      }
    }
    
    
    
  }
  
  
  
      
}
variables<-as.data.frame(variablesA)
names(variables)<-c("num","name","label","type","details")
detailsIC<-as.data.frame(detailsICA)
detailsC<-as.data.frame(detailsCA)
names(detailsIC)<-c("na","nu","min","max")
names(detailsC)<-c("na","nu","min","max")
indivICA<-array(0,c(individualsNo,variablesNo,2))
indivCA<-array(0,c(individualsNo,variablesNo))
CA<-array(0,c(individualsNo,variablesNo,1))
for(i in 1:individualsNo)
{
  for(j in 1:variablesNo)
  {
    if(rsda.object$sym.var.types[j]=="$I"){
      indivICA[i,j,1]<-rsda.object$meta[i,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[j])[1]]
      indivICA[i,j,2]<-rsda.object$meta[i,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[j])[2]]
    }
    if(rsda.object$sym.var.types[j]=="$C"){
      indivCA[i,j]<-rsda.object$meta[i,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[j])[1]]
    }
  }
}
if(!is.null(detailsListNom)){
  
  colnames(detailsListNom)<-c("details_no","num","name","label")
  colnames(detailsNA)<-c("na","nu","modals")
  detailsListNom<-as.data.frame(detailsListNom)
  detailsNA<-as.data.frame(detailsNA)
}
if (!is.null(indivNA))
{
  indivNA<-as.data.frame(indivNA)
  names(indivNA)<-c("indiv","variable","value")
}

if (!is.null(indivNMA))
{
  indivNMA<-as.data.frame(indivNMA)
  names(indivNMA)<-c("indiv","variable","value","frequency")
}

resul<-list(individuals=indiv,variables=variables,detailsIC=detailsIC,detailsC=detailsC,detailsN=detailsNA,detailsListNom=detailsListNom,detailsNM=NULL,detailsListNomModif=NULL,indivIC=indivICA,indivC=indivCA,indivN=indivNA,indivListNom=NULL,indivNM=NULL,indivListNomModif=indivNMA)

class(resul)<-"symbolic"
resul
}