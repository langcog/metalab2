## pulls out all citations from rmd ("@whatever)") and formats them into latex citation command


library(stringr)


fileName <- "rawmetalabtext.txt"
m = readChar(fileName, file.info(fileName)$size)

citation <-  "@[:alnum:]*"
k = unlist(str_extract_all(m, citation)) 
p = sub("@", "", k)

paste("\nocite{", paste(p, collapse=","), "}" ,sep = "")

