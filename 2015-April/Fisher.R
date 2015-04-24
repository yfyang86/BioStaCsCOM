#Rscript hyp...R "/home/mcg/..." "/home/mcg/....."

#Args <- commandArgs(TRUE);
InFile<-"C:\\Users\\ThinkPad User\\Desktop\\New folder\\a";
OutFile<-"C:\\Users\\ThinkPad User\\Desktop\\New folder\\b";

makeTable <- function(x){
	y <- matrix(x,ncol=2,byrow=F);
	y[,1] <- y[,1]-y[,2];
	return (y);
}

input <- as.matrix(read.csv(file=InFile,header=F,sep='\t'));
pvalue <- NULL;
n <- nrow(input);
for(i in 1:n){
	result <- fisher.test(makeTable(as.double(input[i,2:5])));
	pvalue <- c(pvalue,result$p.value);
	padj <- p.adjust(pvalue, method = "bonferroni", n = length(pvalue))
}

fold <- NULL;
fold <- (((as.double(input[,2]))*(as.double(input[,5])))/((as.double(input[,3]))*(as.double(input[,4]))));

write.table(cbind(input,fold,pvalue,padj),file=OutFile,row.names=F,col.names=F,quote=F,sep='\t');
