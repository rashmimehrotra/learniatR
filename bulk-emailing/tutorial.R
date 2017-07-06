suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(readr)
library(formattable)

## edit line below to reflect YOUR json credential filename
use_secret_file("sm-secret.json")
load("~/Dropbox/R-wd/learniat_variables")

d$tbl_auth %>% 
  merge(d$student_index,by.x="user_id",by.y="student_id") %>%
  group_by(user_id,first_name,email_id) %>% 
  summarise_at(vars(subtotal_of_score,subtotal_of_count),funs(mean,sum)) ->score_table
  
this_hw <- "The Fellowship Of The Ring"
email_sender <- 'Stone Hill Learniat <sanjmeh@gmail.com>' 
optional_bcc <- 'rashmimeh@gmail.com>'    
body <- "Hi, %s.
Your Grasp Index as of today is %s.
Thanks for participating in the class!
"

edat <- score_table %>%
  mutate(
    To = sprintf('%s <%s>', first_name, email_id),
    From = "sanjmeh@gmail.com",
    Subject = sprintf('Your total count of interactions till date is %s', subtotal_of_count_sum),
    body = sprintf(body, first_name,percent(subtotal_of_score_mean),2)) %>%
  select(To, From, Subject, body)
edat
write_csv(edat, "composed-emails.csv")

emails <- edat %>%
  pmap(mime)

## optional: use if you've created your own client id
# use_secret_file("gmailr-tutorial.json")
# 
# safe_send_message <- safely(send_message)
# sent_mail <- emails %>%
#   map(safe_send_message)
# 
# saveRDS(sent_mail,
#         paste(gsub("\\s+", "_", this_hw), "sent-emails.rds", sep = "_"))
# 
# errors <- sent_mail %>%
#   transpose() %>%
#   .$error %>%
#   map_lgl(Negate(is.null))
# sent_mail[errors]