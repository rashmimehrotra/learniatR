library(magrittr)
library(RMySQL)
library(DBI)
library(stringr)
library(formattable)
library(XML)
library(dplyr)
library(lubridate)
library(tibble)

flag <- 0

if(flag==1) database_name="jupiter" else database_name="jupiter_dev"

host_address="54.251.104.13"
db_user_name <-'sanjay'
db_password<-'hello22'

list_of_functions<-function(){
    lines<-scan(file="~/Dropbox/R-wd/quick.R",what=character(),skip = 20)
    grepl("function",lines)
    i<-grepl("function",lines)
    fns_grouped<-{lines[i] %>% strsplit("<-")}
    listfns<-NULL
    for(i in 1: NROW(fns_grouped)){
        listfns<-c(listfns,fns_grouped[[i]][1])
    }
    sort(listfns)
}

tail<-function(tab="topic_log",col=stop("You have to give a column name")){
    table_all(tab) %>% arrange(desc(col)) %>% head()
}
    
    
table_all("topic_log") %>% arrange(desc(topic_id)) %>% head()


start_classes<-function(class=14,t=496,starts=0.2,size=1,number=3,gaps=0.25,room=20){
    run_SQL(GS_insert_class_sessions(class_id=class,teacher_id = t, gaps = gaps,number_rows = number,start_after = starts,duration = size,room_id = room))
}

uname<-function(name="Edna"){
table_all("tbl_auth") %>% filter(grepl(name,user_name))
}

GS_insert_class_sessions<-function(number_rows=1,start_after=1, 
                                   duration=0.5, gaps = 0.5,class_id=14,room_id=25,teacher_id=496){
    
    query_text<-"INSERT INTO class_sessions (class_id,room_id,teacher_id,starts_on,ends_on,session_state)"
    value_text1<- paste0(" VALUES(",class_id, ",", room_id, ",", teacher_id, ", '", starting_time<-now() + dhours(start_after),"', '", ending_time<-starting_time+dhours(duration),"',4)")
    value_text_rest <-NULL
    for(i in 1:number_rows){
        value_text_rest<-paste0(value_text_rest, ", (", class_id, ",", room_id, ",",teacher_id,", '", starting_time<-ending_time + dhours(gaps), "', '", ending_time<-starting_time +dhours(duration),"',4)")
    }
    paste0(query_text,value_text1,value_text_rest)
}

xmpp_archive<-function(number_rows=20,width=80,IST=T){
sql_text<-paste0("select * from archive order by created_at desc limit ",number_rows)
top_rows<-suppressWarnings(query_SQL(sql_text,database = "ejabberd_1609"))
top_rows$txt<-str_sub( top_rows$txt,1,width)
if (IST) top_rows$created_at %<>% ymd_hms() %<>% +dhours(5.5)
top_rows %>% select(1,4,6,10)

}

api_archive<-function(number_rows=20,max_width=80,IST=T){
    sql_text<-paste0("select * from event_log order by request_time desc limit ",number_rows)  
    top_rows<-suppressWarnings(query_SQL(sql_text,database = database_name))
    top_rows$xml_output<-str_sub( top_rows$xml_output,1,max_width)
    top_rows$xml_input<-str_sub( top_rows$xml_input,1,max_width)
    if (IST) {
        top_rows$request_time %<>% ymd_hms() %<>% +dhours(5.5)
        top_rows$return_time %<>% ymd_hms() %<>% +dhours(5.5)
    }
    top_rows$time_taken<-top_rows$return_time - top_rows$request_time
   # top_rows %>% mutate(input= function(xml_input) {%>% .[[1]] %>% .[[1]]})
    top_rows
}

api_xml<-function(number=87788,rows=500,max_width=500){
    x<-api_archive(rows,max_width)
    input_xml_structured <- x %>% filter(event_log_id==number) %>% .[,5] %>% xmlTreeParse() %>% .[[1]] %>% .[[1]]
    output_xml_structured<- x %>% filter(event_log_id==number) %>% .[,7] %>% xmlTreeParse() %>% .[[1]] %>% .[[1]]
    print(input_xml_structured)
    print("--------")
    print(output_xml_structured)
    
}



show_sessions<-function(number=20,tz=5.5,user="ALL"){
    if(user=="ALL"){
    sql_text<-paste("Select * from class_sessions order by starts_on desc limit ",number)
    x<-query_SQL(sql_text)
    x$starts_on%<>% ymd_hms()
    x$ends_on%<>% ymd_hms()
    } else if(is.numeric(user)){
    show_sessions(tz=tz,number=10,user = "ALL") %>% filter(class_session_id %in% getMyTodaysSessionIDs(user)$class_session_id)->x
    }
    x
}



show_sessions2<-function(number=10){
show_sessions(number=number) %>% merge(table_all("classes"),by = "class_id") %>% select(1:8,14,17,20) %>% merge(table_all("subjects"))
}

list_students<-function(class_id=20){
    query_SQL(paste('SELECT student_id FROM student_class_map WHERE class_id=',class_id)) %>% merge(user_status(1:1000),by.x="student_id",by.y="user_id")
}

list_XMPP_users<-function() {
    script<-"SELECT * FROM users"
    x<-suppressWarnings( query_SQL(script,database="ejabberd_1609"))
    x[,1:2]
}



questions<-function(topic=32){
    script<-paste("select * from questions where topic_id=",topic," order by question_id")
    q<-suppressWarnings(query_SQL(script))
    q %>% select(1,2,3,6)
}

queries<-function(){
    script<-paste("select * from student_query order by query_id desc limit 10")
    q<-suppressWarnings(query_SQL(script))
    q
}

user_status<-function(users=c(496,524,529,532,540,544,545)) {
   list_users() %>% filter(user_id %in% users)
}

delete_session<-function(session_id=stop("you have to provide the numeric session_id")){
    script<-paste("DELETE from class_sessions where class_session_id=",session_id)
    deleted_sessions<-run_SQL(script,database_name)
    message(deleted_sessions,":rows deleted from class_session_id")
}


list_users<-function() {
    script<-"SELECT * FROM tbl_auth"
    x<-suppressWarnings( query_SQL(script,database=database_name))
    x[c(1:2,4:7,11)]
}

state_transitions<-function(numbers=10,IST=T){
    sql_text<-paste("select * from state_transitions order by transition_time desc limit ",numbers)
    x<-suppressWarnings(query_SQL(sql_text))
    if (IST) x$transition_time%<>% ymd_hms() %<>% +dhours(5.5)
    x
}

password_mismatches<-function() {
    xmpp_users<-list_XMPP_users()
    users<-list_users()
    merge(xmpp_users,users,all=TRUE,by.x="username",by.y="user_id") %>% filter(!password.x==password.y) %>% 
        select(5,1:2,6,3:4,7)
}


password_sync<-function() {
    x<-password_mismatches()
    sql_text<-paste0("UPDATE users SET password=", "'" ,x$password.y, "'"  ,  " WHERE username=", x$username, ";")
    number_updated<-run_SQL(sql_text,"ejabberd_1609")
     cat(paste(number_updated,"password(s) updated in XMPP server"))
    #cat(sql_text)
}
    


topics_inClass<-function(class_id=14,tag=1){
    ifelse(tag<2,
           sql_text<-paste0("SELECT * FROM topic where topic_id in (select topic_id from lesson_plan where class_id=",class_id," and topic_tagged=",tag,")"),
           sql_text<-paste0("SELECT * FROM topic where topic_id in (select topic_id from lesson_plan where class_id=",class_id, ")")
    )
           
    t<-suppressWarnings(  query_SQL(sql_text) )
    t2<-t
    suppressWarnings( merge(t,t2,by.x = "parent_topic_id",by.y = "topic_id")) ->t3
    t3[,!duplicated(colnames(t3))] %>% select(13,1,5,2,8:10) %>% rename(Main_topic=topic_name.y,Maintopic_id=parent_topic_id,SubTopic=topic_name.x,SubTopic_id=topic_id,GI_num=gi_num.x,GI_den=gi_den.x,PI=pi.x)

}

main_topics_inClass<-function(class_id=14){
    topics_inClass(class_id) %>% group_by(Main_topic) %>% tally %>% rename(Count_SubTopics=n) %>% print(n=Inf)

}

main_topics<-function(){
    t<-suppressWarnings(query_SQL("SELECT * from topic"))
    s<-suppressWarnings(query_SQL("SELECT * from subjects"))
    t %>% filter(is.na(parent_topic_id)) %>% merge(s) %>% rename(MainTopic=topic_name) %>% select(subject_name,MainTopic,topic_id) %>% arrange(subject_name,topic_id)
}

subtopics_all<-function(){
    t<-suppressWarnings(query_SQL("SELECT * from topic"))
    s<-suppressWarnings(query_SQL("SELECT * from subjects"))
    t2<-t
    suppressWarnings( merge(t,t2,by.x = "parent_topic_id",by.y = "topic_id")) ->t3
    t3[,!duplicated(colnames(t3))] %>% select(13,1,5,2,8:10) %>% rename(Main_topic=topic_name.y,Maintopic_id=parent_topic_id,SubTopic=topic_name.x,SubTopic_id=topic_id,GI_num=gi_num.x,GI_den=gi_den.x,PI=pi.x)
}
    
tables<-function(dbname=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address)  # connect the DB at Amazon
    on.exit(dbDisconnect(db))
    dbListTables(db)
}

cols<-function(table="class_sessions", dbname=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address)  # connect the DB at Amazon
    on.exit(dbDisconnect(db))
    dbListFields(db,table)
}

table_head<-function(table="class_sessions", nrow=20,dbname=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address)  # connect the DB at Amazon
    on.exit(dbDisconnect(db))
    suppressWarnings(    dbReadTable(db,table)) %>% head(nrow)
}

table_all<-function(table="class_sessions",dbname=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address)  # connect the DB at Amazon
    on.exit(dbDisconnect(db))
    suppressWarnings(    dbReadTable(db,table))
}

disconnect<-function(db=dbs()[[1]]){
    dbDisconnect(db)
}

GS_insert_subtopic<-function(parent_topic=stop("please provide main topic_id"),
                             sub_topic=stop("please provide sub topic_id"),topic_name=stop("new subtopic name is mandatory")){
    
    query_text<-"INSERT INTO topic (topic_id,parent_topic_id,topic_name)"
    value_text1<- paste("VALUES(",sub_topic,",",parent_topic,",",topic_name,")")
    paste(query_text,value_text1)
}
#------
run_SQL<-function(sql_text=stop("Please provide an SQL script starting with INSERT or UPDATE as first parameter and a database name as second. If database = 'jupiter_dev' you can skip"),database=database_name){
    db<-dbConnect(MySQL(),user=db_user_name,password=db_password,dbname=database,
                  host=host_address)  # connect the DB at Amazon
    on.exit(dbDisconnect(db))
    rows_affected<-dbExecute(db,sql_text)
    rows_affected
    }


query_SQL<-function(sql_text="SELECT * from classes",database=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=database,
                  host=host_address)  # connect the DB at Amazon
    on.exit(dbDisconnect(db))
    dbGetQuery(db,sql_text)
}

#-----

dbs<-function(){
    dbListConnections(MySQL())
}

php_log<-function(date=23,month="jun"){
url_string<-paste0("http://54.251.104.13/learniat/application/logs/log-2017-06-",date,".log")    
con_log<-url(url_string)
x<-readLines(con_log)
close.connection(con_log)
x
}

#---- API local copies

login<-function(app_id=stop("app_id is mandatory"), user_name=stop("Usermame is mandatory"),pass=stop("password is mandatory"),uuid="NOT PASSED",lat=0,long=0,app_version=0)
{ 
    input_df=data.frame(app_id=app_id,user_name=user_name,password=pass,UUID=uuid,lat=lat,long=long,app_version=app_version)
    request_time<-now()
    output_df=data.frame(Status=character(1),UserId=numeric(1), SchoolId=numeric(1),warning_message=character(1),last_updated=as.Date("2017-01-01", origin = "1900-01-01"),error_message=character(1),existing_state=numeric(1),stringsAsFactors=F)
    db = dbConnect(MySQL(), user=db_user_name, password=db_password, dbname=database_name, host=host_address)
    on.exit(dbDisconnect(db))
    sql <- "SELECT * FROM tbl_auth WHERE user_name = ?name"
    sql_new<-sqlInterpolate(ANSI(), sql, name = user_name)
    user<-suppressWarnings(dbGetQuery(conn=db,statement=sql_new))
    #browser()
    if(nrow(user)==0){ output_df[1,"error_message"]<-dbGetQuery(conn=db,"select * from error_messages where error_code=105")[1,3];output_df[1,"Status"]<-"Error"; return(output_df)}
    password_is_correct=NULL
    if(!user$role_id==app_id)  {
        { output_df[1,"error_message"]<-dbGetQuery(conn=db,"select * from error_messages where error_code=104")[1,3];output_df[1,"Status"]<-"Error"; return(output_df)}
    } else password_is_correct=(user$password==pass)
    output_df$Status<- if(password_is_correct)  "Success" else "Error"
    output_df$UserId<-user$user_id
    output_df$SchoolId<-user$school_id
    if(password_is_correct) {
        connected_dev<-ifelse (is.na(user$connected_device_id),0,user$connected_device_id) 
        if (!input_df$UUID == connected_dev) {output_df$warning_message<-"This user is signed in from another device.. signing in from here also and changing state to 7"
        sql <- "UPDATE tbl_auth SET connected_device_id=?dev, last_updated=?dt WHERE user_id = ?id1;"
        sql_new<-sqlInterpolate(ANSI(),sql,dev=uuid,dt=as.character(now()),id1 = user$user_id)
        x<-dbExecute(db,sql_new)
        } 
        output_df$Status<-"Success"
        output_df$existing_state<-user$user_state
        output_df$last_updated<-user$last_updated
    } else output_df$error_message<-"Incorrect Password"
    
    if(output_df$Status=='Success'){
        #update sign in states in tbl_auth and state_transtions
        sql1 <- "UPDATE tbl_auth SET last_updated=now(), user_state=7 WHERE user_id = ?id1;"
        sql2 <- "INSERT INTO state_transitions VALUES (1, ?id2, 8,7,now())"
        sql_new1<-sqlInterpolate(ANSI(),sql1, id1 = user$user_id)
        sql_new2<-sqlInterpolate(ANSI(), sql2, id2 = user$user_id)
        x1<-dbExecute(db,sql_new1)
        x2<-dbExecute(db,sql_new2)
        if(any(x1>1,x2>1)) output_df$error_message<-"Something seriously wrong has happened, as more than one row was updated in tbl_auth"
    }
    
    x<-update_event_log(db,service_name='login', user_id=user$user_id,      UUID=uuid,         xml_input=paste(input_df[1,],collapse=";"),    request_time=request_time,
                        xml_output=paste(output_df,collapse=";"),   return_time=now() )
    if(!x==1) output_df$warning_message <-"Event log could not be updated or has updated many rows. Please check event_log table in jupiter DB"
    output_df
}


getMyTodaysSessions<-function(user_id=NULL,uuid=NULL)
{ 
    missing_values<-data.frame(SeatsConfigured=25, PreAllocatedSeats=24,OccupiedSeats=5,TimeZone="05:30:00")
    sessions<-show_sessions(user=user_id)
    sessions %>%
        merge(table_all("classes"),by="class_id") %>% 
        merge(table_all("subjects")) %>% 
        merge(table_all("rooms")) %>% 
        merge(table_all("tbl_auth"),by.x="teacher_id.x",by.y = "user_id") %>% 
        merge(missing_values) %>%
        rename(SessionId=class_session_id, ClassName=class_name,ClassId=class_id,TeacherName=first_name,TeacherId=teacher_id.y,SubjectName=subject_name,
               SubjectId=subject_id,StartTime=starts_on,EndTime=ends_on,SessionState=session_state,
               StudentsRegistered=stud_regist,RoomName=room_name,RoomId=room_id,Grade=grade_id.x,StudAttended=stud_attended) %>%
        select(SessionId, TeacherName,SubjectName, SubjectId, ClassName,StartTime,EndTime,SessionState,StudentsRegistered,Grade,StudAttended,RoomName,SeatsConfigured, PreAllocatedSeats,OccupiedSeats,TimeZone)
    # All the output paramter names from xml
    # <SessionId>3924</SessionId>
    #     <StartTime>2017-07-03 08:25:19</StartTime>
    #     <EndTime>2017-07-03 08:55:19</EndTime>
    #     <SessionState>4</SessionState>
    #     <TeacherName>Edna</TeacherName>
    #     <TeacherId>496</TeacherId>
    #     <ClassId>20</ClassId>
    #     <ClassName>9th grade Mathematics section C</ClassName>
    #     <RoomId>25</RoomId>
    #     <RoomName>Room 25A</RoomName>
    #     <SubjectId>1</SubjectId>
    #     <SubjectName>Mathematics</SubjectName>
    #     <SeatsConfigured>15</SeatsConfigured>
    #     <StudentsRegistered>6</StudentsRegistered>
    #     <PreAllocatedSeats>0</PreAllocatedSeats>
    #     <OccupiedSeats>0</OccupiedSeats>
    #     <TimeZone>05:30:00</TimeZone>
    # Input names: 
    # [1] "teacher_id.x"          "room_id"               "school_id.x"           "subject_id"            "class_id"              "class_session_id"      "starts_on"             "ends_on"              
    # [9] "session_state"         "stud_attended"         "total_stud_registered" "session_time"          "gi_num.x"              "gi_den.x"              "pi.x"                  "class_name"           
    # [17] "grade_id.x"            "section_id.x"          "academic_term_id"      "teacher_id.y"          "stud_regist"           "gi_num.y"              "gi_den.y"              "pi.y"                 
    # [25] "subject_name"          "room_name"             "state"                 "last_updated.x"        "first_name"            "middle_name"           "last_name"             "user_name"            
    # [33] "password"              "role_id"               "grade_id.y"            "section_id.y"          "school_id.y"           "user_state"            "email_id"              "notif_email_id"       
    # [41] "connected_device_id"   "latitude"              "longitude"             "last_updated.y"        "user_active"          

        }



session_list<-NULL

refreshMyApp<-function(userid=NULL){
    time_zone<-tz(now())
    current_session<-F
    next_session<-F
    
    if (time_zone=="") warn<-"The time zone of server is not set"
    if(is.null(userid)) return(error_message<-"Mandatory parameter UserId is missing") else
        # extract state from tbl_auth
        user_state<-table_all("tbl_auth") %>% filter(user_id==userid) %>% select(user_state) %>% .[,1]
    MyUserState<-8
    if(NROW(user_state)==1) {
        MyUserState<-user_state
        show_sessions(tz=0,user=userid) ->x
        x %>% select(1,5:6,7) -> session_list
        time<-now()
        m1<-time>session_list$starts_on
        m2<-time>session_list$ends_on
        m3<-xor(m1,m2)
        #browser()
        if (sum(m3)>1) warn<-c(warn,"We have at least one overlapping class session")
        if(any(m3)) session_list[m3,]->current_session2
        if(exists("current_session2")){
            current_session<-TRUE
            MyCurrentSessionDetails<-current_session2
        } else {
            current_session<-FALSE
            MyCurrentSessionDetails<-NA
        }
        
        gaps<-interval(time,session_list$starts_on)
        if(NROW(gaps)) {gaps2<-time_length(gaps); pos<-gaps2[gaps2>0]}
        if (current_session) MyCurrentSessionDetails<-current_session2 else MyCurrentSessionDetails<-NA
        if(exists("pos")) min_pos<-min(pos)
        if(exists("min_pos")) { 
            truth_table<-gaps2==min_pos
            MyNextSessionDetails<-session_list[truth_table,]
            next_session<-TRUE
        } else {next_session<-FALSE;  MyNextSessionDetails<-NA}
       
        NextSession_id<-NA; NextSession_state<-NA
        
        if(next_session) 
        {
            MyNextSessionDetails$class_session_id -> NextSession_id
            MyNextSessionDetails$session_state -> NextSession_state
        }
        
        CurrSession_id<-NA; CurrSession_state<-NA
        if(current_session) 
        {
            CurrSession_state<- MyCurrentSessionDetails$session_state
            CurrSession_id <-MyCurrentSessionDetails$class_session_id
        }
        
        MySessionStates<-session_list[,c(1,4)]
        output<-data.frame(NextClassSessionId=NextSession_id,NextClassSessionState=NextSession_state,MyCurrentState=MyUserState,
                           MyCurrentSessionState=CurrSession_state)
        list(Summary=output,AllSessions=MySessionStates)
    } else return(error_message<-"This user does not exist")
}



InsertScribbleFileName<-function(filename=NULL,user_id=NULL,image_type_id=5,uuid=NULL){
    if (any(is.null(filename),is.null(user_id))) return(error_message<-"filename and userid are mandatory")
    script<-"insert into uploaded_images (image_type_id,image_path,uploaded_by,active,DateUploaded) VALUES (?x1, ?x2, ?x3, 1,?x4 )"
    sql<-sqlInterpolate(ANSI(),script,x1=image_type_id,x2=filename,x3=user_id,x4=as.character(now()))
                 
    x<-run_SQL(sql)
    retrieve_script<-"select image_id from uploaded_images where image_path = ?path"
    sql<-sqlInterpolate(ANSI(),retrieve_script,path=filename)
    x<-query_SQL(sql)
    x
}



#---this functions are exlusively called from APIs


update_event_log<-function(db,service_name='UNKNOWN', user_id=0,      UUID=0,         xml_input='XYZ',    request_time=now(),
                           xml_output='Nothing',   return_time=now()){
    sql <- "INSERT INTO event_log values  (0,?service_name, ?user_id,  ?UUID, ?xml_input, ?request_time, ?xml_output, ?return_time, 'json'  )"
    sql_new<-sqlInterpolate(ANSI(), sql, service_name=service_name, user_id=user_id,      UUID=UUID,         xml_input=xml_input,    request_time=as.character(request_time), xml_output=xml_output,   return_time=as.character(return_time))
    x<-dbExecute(db,sql_new)
    x
}

getMyTodaysSessionIDs<-function(UserId=496){
    role_id<-user_status(1:1000) %>% filter(user_id==UserId) %>% .[,6]
    if(role_id==4){
        sql1<-"SELECT DISTINCT(class_id) FROM student_class_map WHERE student_id =?id"
        sql2<-paste("SELECT class_session_id FROM class_sessions WHERE date(starts_on) = date(now()) AND class_id IN (", sql1,")")
        sql_new<-sqlInterpolate(ANSI(), sql2, id = UserId)
        sessions<-query_SQL(sql_new)
    } else  if(role_id==3){
        sql<-paste("SELECT class_session_id FROM class_sessions WHERE date(starts_on) = date(now()) AND  teacher_id=",UserId)
        sessions<-query_SQL(sql)
    }
    sessions
}


