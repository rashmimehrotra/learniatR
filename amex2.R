{library(pdftools)
  library(stringr)
  library(lubridate)
  library(gridExtra)
  library(formattable)
}
rm(list=ls())
setwd("~/Dropbox/R-wd")
amex<-list.files("~/Downloads","^Statement_[A-Z][a-z]{1}.*201[0-9]\\.pdf",full.names = T)
amexfiles<-list.files("~/Downloads","^Statement_[A-Z][a-z]{1}.*201[0-9]\\.pdf",full.names = F)
load("~/amex.Rdata")
len=length(amex)

rmNA <- function(sent){
  NArows<-is.na(sent[,1])
  k2<-gsub("\\s+"," ",sent[!NArows,])
  k2
}

if(!identical(amex,amex.backup)){
  message("Detected a change in ~/Downloads. Picking all the amex files again....")
  amex.backup<-amex
  df<-data.frame(Statement_Year=rep(NA,len),Statement_Month=rep(factor(NA,levels = month.abb),len),
                 Num_pages=rep(NA,len),Amount_paid=rep(00.00,len),Card_number=NA,row.names = amexfiles)
  amex_info<-lapply(amex,pdf_info)
  amex_text<-lapply(amex,pdf_text)
  sentz2<-sapply(amex_text,str_split,pattern=boundary(type = "sentence"))
  message("done")
}


dt<-NULL
descr<-NULL
amt<-NULL

cat("starting to loop through file number....")

mths<-month.name
m<-factor(NULL,levels=mths,ordered = T)

for (i in 1: len) {
  cat(i," ")
  page1<-sentz2[[i]][[1]]
  page1<-gsub("\\s+"," ",page1)
  statement_period<-str_match(page1,"Statement Period From ([A-Z].+\\s+[0-3][0-9]) to ([A-Z].+\\s+[0-3][0-9]), (201[0-9])")
  statement_period<-rmNA(statement_period)
  stopifnot(length(statement_period)==4)
  words<-str_extract(statement_period,"\\w+")
  m[1]=words[2]
  m[2]=words[3]
  start_month<-m[1]
  end_month<-m[2]
  year<-as.numeric(words[4])
  if (end_month < start_month ) {year_start<-year-1 } else
    year_start<-year
  
  statement_period_start<-ymd(paste(year_start,statement_period[2]))
  statement_period_end<-ymd(paste(year,statement_period[3]))
  
  # loop through each page of the statement number i
  for (j in 1:as.integer(amex_info[[i]][[2]])) {
    #call a function to clean up the regex output by compressing all white spaces and filtering out the NA rows
    k<-str_trim(sentz2[[i]][[j]])
    rows_with_CR<-grep("CR$", k)
    if(length(rows_with_CR)>0) {
      for (row_no in rows_with_CR){
        k[row_no-1]<-gsub("([A-Z][a-z]+\\s[0-9]{1,2})(\\s+[A-Z0-9].*\\s+)([0-9,\\.]+$)","\\1\\2-\\3",k[row_no-1])
        #replace all rows just before the characters CR are seen as negative number (CREDIT) in column 3
      }
    }
    k1<-str_match(k,"([A-Z][a-z]+\\s[0-9]{1,2})(\\s+[A-Z0-9].*\\s+)([\\-0-9,\\.]+$)")
    k2<-rmNA(k1)
    
    
    
    
    # process the above matrix only if it has some data
    if (ifelse(is.null(dim(k2)),FALSE,nrow(k2))>0){
    
      trx_on_page<- nrow(k2)
      years<-rep(year,trx_on_page) #set all transaction year on the page to "year of statement"
      
      #split the transactions with  month January and non January
      
       # x<-str_extract(k2[,2],"\\w+")=="January" #x is a logical vector with Jan dates
        #y<-!str_extract(k2[,2],"\\w+")=="January" #y is a logical vector with non Jan dates
      m<- factor(str_extract(k2[,2],"\\w+"),levels = month.name,ordered = T)
      years_revised<-ifelse((m>end_month),years-1,years)
      
        #concatenate date into a single vector; this will group Dec and Jan in a sequence. 
        # If the statement has jumbled up dates then the dates of some transactions would be shown incorrect (swapped)
      date_string<-paste(as.numeric(years_revised),k2[,2])
      date_ymd<-ymd(date_string)
      concatenated_dates<-c(dt,date_ymd)
      dt<-as.Date(concatenated_dates,origin = "1970-01-01")
      
      
      #concatenate desc and amt into a single vector each
      descr<-c(descr,k2[,3]) #add descr into one variable
      amt<-c(amt,k2[,4]) # add amt as one variable
    }
  }
}
  #convert amt to accounting format
  # amt2<-accounting(amt)
  dt<-as.Date(dt,origin = "1970-01-01") #convert the date char vector into a Date vector
  
  #convert the vectors into a dataframe
  df1<-data.frame(transaction_date=dt,particulars=descr,Amount=amt)
  
  
  #remove unwanted rows with particulars not specific to any transactions
  #notwantedlevels<-c(57,86,87)
  #df2<-df1[!(as.integer(df1$particulars) %in% notwantedlevels),]
  
  #order on date and then on particulars
  ordered_seq<-order(df1[,1],df1[,2])
  df2<-df1[ordered_seq,]
  
  #sequence the rownames
  rownames(df2)<-seq_along(df2[,1])
  
  #save important objects for next time
  save(amex.backup,sentz2,amex_info,amex_text, file="~/amex.RData")
  
  #send output on terminal
  print(df2)
  