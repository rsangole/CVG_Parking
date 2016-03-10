setwd("~/Documents/Data Science/CVG_Parking")
library('XML')
url <- "http://www.cvgairport.com/"
con <- url(url)
htmlCode <- readLines(con)
close(con)

htmlCode <- htmlTreeParse(url,useInternalNodes = T)

count <- xpathSApply(htmlCode,'//*[contains(concat( " ", @class, " " ), concat( " ", "parking-number", " " ))]',xmlValue)

termgarage <- count[1]
valupark <- count[2]

termgarage <- strsplit(termgarage,split = '/ ')
valupark <- strsplit(valupark,split = '/ ')

termgarage <- as.numeric(sub(x = termgarage[[1]],pattern = ',',replacement = ''))
valupark <- as.numeric(sub(x = valupark[[1]],pattern = ',',replacement = ''))

termgarage.pc <- round(termgarage[1]/termgarage[2]*100,digits = 3)
valupark.pc <- round(valupark[1]/valupark[2]*100,digits=3)

x <- data.frame(Sys.time(),termgarage.pc,valupark.pc)

write.table(x,file = 'o.txt',append = T,sep = ',',row.names = F,col.names = F,quote = F)