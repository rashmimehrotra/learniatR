{library(pdftools)
  library(dplyr)
    library(stringr)
    library(lubridate)
    library(formattable)
}


rm(list=ls())
setwd("~/Dropbox/R-wd")

# BANK_ACCT_NAMES <-c("Citibank-SANJAY-SAVINGS", "HDFC-RASHMI-MAIN","HDFC-VIBHU", "SBI","AXIS-BANK-NEW" )
# save(BANK_ACCT_NAMES,file="bank_constants")

if(file.exists("bank_constants")) load("bank_constants") else {
  message("File: `bank_constants` not found.")
}

rmNA <- function(sent){
    NArows<-is.na(sent[,1])
    k2<-gsub("\\s+"," ",sent[!NArows,])
    k2
}


citi<-list.files("~/Downloads","^Citibank Account Statement-.*\\.pdf",full.names = T)
citifiles<-list.files("~/Downloads","^Citibank Account Statement-.*\\.pdf",full.names = T)
# load("~/citi.Rdata")
len=length(citi)

print(citifiles)
file_number<-as.numeric(readline(prompt="enter the serial number of the file you want to process: "))
password<-readline(prompt=paste("password to read the file",citifiles[file_number],": "))
password<-ifelse (str_count(password)<8, "4AGZAGZ164PT3",password)

cat(paste("file name:",citifiles[file_number],"\n","Password:",password ))


conf<-readline(prompt="proceed? Y/N: ")

if(conf=="Y"){
    
    
    citi_info<-pdf_info(citifiles[file_number],opw=password)
    total_pages<-as.integer(citi_info[[2]]) #the pdf_info extracts a list that contains page numbers as second element.
    citi_text<-pdf_text(citifiles[file_number],opw=password) # there is bug in the package pdftools and if opw is incorrect it crashes the R session
    sentz<-sapply(citi_text,str_split,pattern=boundary(type = "sentence")) #break the pages into lists of sentences
    

#initialize an empty dataframe
    df<-NULL
    df <- data.frame(Date=integer(),
                     Description=character(),
                     Withdrawals=double(),
                     Deposits=double(),
                     Balance=double(),
                     stringsAsFactors=FALSE)

    
    
    
    #just another way to initialise a dataframe
    colclasses<- c("Date", "character", "double","double","double")
    df1<-df <- read.csv(text="Date,Description,Withdrawals,Deposits,Balance", colClasses = colclasses)
    
    serialno<-1
    last_serial<-NULL
    Openingbalance<-NULL
    withdrawal<-NA;deposit<-NA
    
    # loop through each page of the statement number
    for (page_no in 1:total_pages) {
        page<-sentz[[page_no]]
        page<-gsub("\\s+"," ",page)
        # page<-gsub("^\\s+","",page)
        # page<-gsub("\\s+$","",page)
        page<-str_trim(page,side = "both")
        
        if(page_no==1){ #extract statement period from page-1
            
            statement_period<-str_match(page,"Statement Period: ([A-Z].+\\s+[0-3][0-9][,]201[2-9])\\s+to\\s+([A-Z].+\\s+[0-3][0-9][,]201[2-9])")
            statement_period<-rmNA(statement_period)
            stopifnot(length(statement_period)==3)
            statement_period_start<-mdy(statement_period[2])
            statement_period_end<- mdy(statement_period[3])
            
            Openingbalance_array<-str_match(page,"Opening Balance:\\s+([0-9.]+)" )
            stopifnot(length(Openingbalance<-rmNA(Openingbalance_array))==2)
            Openingbalance<-as.numeric(Openingbalance[2])
            balance<-Openingbalance
        }
        
        rows_with_dates<-grep("^[0-3][0-9]\\/[01][0-9]",page)
        
        shifted_rows<-rows_with_dates[2:length(rows_with_dates)]
        shifted_rows-1->shifted_rows
        tot_rows<-length(rows_with_dates)
        ranges<-matrix(c(rows_with_dates[1:tot_rows-1],shifted_rows),ncol=2)
        ranges<-rbind(ranges,c(x<-ranges[tot_rows-1,2]+1,x+2))
        start_serial<-serialno
        if(tot_rows>0) {
            for (row_no in rows_with_dates){
                dt<-dmy(substr(page[row_no],1,10))
                df[serialno,"Date"]<-dt
                last_serial<-serialno
                serialno<-serialno+1
            }
            serialno<-start_serial
            startrow<-rows_with_dates[1]
            endrow<-rows_with_dates[tot_rows]+2
            for (rn in startrow:endrow) {
               
                amt<-rmNA(str_match(page[rn],"(\\d[0-9]+[.][0-9]{2})\\s+(\\d[0-9]+[.][0-9]{2})"))
                #print(paste(rn,":",amt[2]))
                if (!is.na(amt[2])){
                    newbalance<- as.numeric(amt[3])
                    if(newbalance<balance) 
                        withdrawal <- as.numeric(amt[2]) else 
                            deposit<-as.numeric(amt[2])
                    balance<-newbalance
                    df[serialno,"Withdrawals"]<-withdrawal
                    df[serialno,"Deposits"]<-deposit
                    df[serialno,"Balance"]<-balance
                    withdrawal<-NA;deposit<-NA
                    df$Withdrawals<-comma(df$Withdrawals)
                    df$Deposits<-comma(df$Deposits)
                    df$Balance<-comma(df$Balance)
                    serialno<-serialno+1
                   
                    #print(paste(serialno,"::",dt,withdrawal,deposit,balance))
                }
            }
        }
        
        
        serialno<-start_serial
        for (sn in 1:nrow(ranges)){
            descr<-NULL
            for(rows in ranges[sn,1]:ranges[sn,2]){
                descr<-paste(descr,sub("(.{12})(.{52})(.{10}.+$)","\\2",sentz[[page_no]][rows]))
                descr<-str_trim(gsub("\\s+"," ",descr),side="both")
            }
            print(paste(page_no,"::",serialno,":",descr))
            df[serialno,"Description"]<-descr
            serialno<-serialno+1
        }
        
    }
 
    attr(df,"class")<-c(attr(df,"class"),"bank_statement")
    sum_of_columns<-comma(colSums(df[1:nrow(df)-1,3:4],na.rm = T))
    #attributes(df)$Summary_statement=list(sum_of_columns[1],sum_of_columns[2])
    closingbalance<-df$Balance[nrow(df)-1]
    attributes(df)$Summary_statement=list(Openingbalance=comma(Openingbalance),sum_of_columns,statement_period[1],ClosingBalance=closingbalance)

    if(exists("BANK_ACCT_NAMES")) attr(df,"Bank")<-BANK_ACCT_NAMES[1] else attr(df,"Bank")<-"CitiBank"
    comment(df)<-"This is processed output from PDF file of statement sent by Citibank. Coding time: 10 hours"
    bank_statement<-df
    save.image(file="~/citi.RData")
    
} else
    message("aborted")


