install.packages("shiny")
install.packages("shinydashboard")
install.packages("tidyverse")
install.packages("rvest")
install.packages("robotstxt")
install.packages("lubridate")
install.packages("stringr")
install.packages("jsonlite")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(robotstxt)
library(lubridate)
library(stringr)
library(jsonlite)

# 1, webscraping processes
# webscraping with rvest package, some regular expressions are used.
#
# website 1
# For website 1, AQI information(today, tomorrow) for Pittsburgh area and Liberty-Clairton area are scarped.
# The content of the website would be updated at ___ everyday, and does not change until next update.
# The source of the websites is the api of airnow.
#
scrape = function(){
webpage1_1 <- read_html("http://feeds.airnowapi.org/rss/forecast/113.xml") # read the website, AQI of Pittsburgh Area
pitt = webpage1_1 %>%
  html_nodes("body") %>% # read the body of the website
  html_text() # extract the text from the website
todaypitt=str_extract(pitt,"(?<=Today).*?(?=AQI)") # extract the string start by Today and end by AQI(e.g.: Today, 03/20/2022: Good - 37 AQI)
todaypitt=strsplit(todaypitt,"-") # split the string by "-"
todaypitt=strsplit(todaypitt[[1]][2]," ") # split again by " "
todaypitt=todaypitt[[1]][2] # select the value of AQI(e.g.: 37)

tomorrowpitt=str_extract(pitt,"(?<=Tomorrow).*?(?=AQI)") # same process, but extracting the AQI of tomorrow.
tomorrowpitt=strsplit(tomorrowpitt,"-")
tomorrowpitt=strsplit(tomorrowpitt[[1]][2]," ")
tomorrowpitt=tomorrowpitt[[1]][2]
# scrape AQI of Liberty-Clairton Area, using the same process above.
webpage1_2 <- read_html("http://feeds.airnowapi.org/rss/forecast/352.xml") 
LC = webpage1_2 %>%
  html_nodes("body") %>%
  html_text()
todayLC=str_extract(LC,"(?<=Today).*?(?=AQI)")
todayLC=strsplit(todayLC,"-")
todayLC=strsplit(todayLC[[1]][2]," ")
todayLC=todayLC[[1]][2]
tomorrowLC=str_extract(LC,"(?<=Tomorrow).*?(?=AQI)")
tomorrowLC=strsplit(tomorrowLC,"-")
tomorrowLC=strsplit(tomorrowLC[[1]][2]," ")
tomorrowLC=tomorrowLC[[1]][2]


# website 2
#
# For this website, we scraped the Discussion part from the report "Southwest Ozone/PM2.5 Forecast", in our report
# it is the content in "Today's Forecast" section.
# The content of the website would be updated at ___ everyday, and does not change until next update.
# The source of the website is the Pennsylvania Department of Environmental Protection.
# 
webpage2 <- read_html("https://www.ahs.dep.pa.gov/AQPartnersWeb/forecast.aspx?vargroup=sw") # read the webpage
todayforecast <- webpage2 %>%
  html_nodes("body form div div div div div div") %>%
  html_text() # obtain all the information of today forecast in text form
discriptions = todayforecast[1]
todayforecast = todayforecast[2] # this is the content we need
todayforecast = strsplit(todayforecast, "\r\n") # removing the irrelevant expression "\r\n"
todayforecast = todayforecast[[1]][2]
todayforecast = gsub("  ","",todayforecast) # removing extra space

# website 3
# 
# The wind direction and speed information is obtained from this website.
# The source is National Weather Service Forecast office.
# The content in this website updates every hour, and only contains the information in the future.
# Better be scraped in a fixed time daily(e.g.: 8:30 am)
# 
webpage4 <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.427&lon=-80.0107&lg=english&&FcstType=digital") # read the website
date = as.character(Sys.Date()) # read system date
date = strsplit(date,"-")# split the sysem time and get the date
date = paste(date[[1]][2],date[[1]][3],sep="/") # reformat the date
p2 <- webpage4 %>% # scarpe the table in the website
  html_nodes(xpath="/html/body/table[6]/tr/td") %>%
  html_text()
p2 = p2[-c(1,402)] # removing irrelevant entries
dim(p2) = c(25,32) # reshape the table
p2 = t(p2) # transpose the table
booldate = FALSE
if(p2[1,2]==date){
  booldate = TRUE
}
time = as.numeric(p2[2,2])# this time need to be <=9, which means this code should be run around 8:30 am everyday
todaymorningwind = "--"
todayafternoonwind = "--"
todayeveningwind = "--"
todayovernightwind = "--"
tomorrowmorningwind = "--"
tomorrowafternoonwind = "--"
# read the table from the websites with different url, so that we can get the table today and tomorrow
web4_1 = paste("https://forecast.weather.gov/MapClick.php?w0=t&w1=td&w2=wc&w3=sfcwind&w3u=1&w4=sky&w5=pop&w6=rh&w7=rain&w8=thunder&w9=snow&w10=fzg&w11=sleet&w13u=0&w16u=1&w17u=1&AheadHour=",0,"&Submit=Submit&FcstType=digital&textField1=40.427&textField2=-80.0107&site=all&unit=0&dd=&bw=",sep="")
web4_2 = paste("https://forecast.weather.gov/MapClick.php?w0=t&w1=td&w2=wc&w3=sfcwind&w3u=1&w4=sky&w5=pop&w6=rh&w7=rain&w8=thunder&w9=snow&w10=fzg&w11=sleet&w13u=0&w16u=1&w17u=1&AheadHour=",24,"&Submit=Submit&FcstType=digital&textField1=40.427&textField2=-80.0107&site=all&unit=0&dd=&bw=",sep="")
# using the same way to reformat the table
webpage4_1=read_html(web4_1)
webpage4_2=read_html(web4_2)
p4_1 <- webpage4_1 %>%
  html_nodes(xpath="/html/body/table[6]/tr/td") %>%
  html_text()
p4_1 = p4_1[-c(1,402)]
dim(p4_1) = c(25,32)
p4_1 = t(p4_1)
p4_2 <- webpage4_2 %>%
  html_nodes(xpath="/html/body/table[6]/tr/td") %>%
  html_text()
p4_2 = p4_2[-c(1,402)]
dim(p4_2) = c(25,32)
p4_2 = t(p4_2)
# obtaining and reformatting the entries we need from the tables
todaymorningwind = paste(p4_1[7,2],p4_1[6,2],sep=" - ")
todayafternoonwind = paste(p4_1[7,8],p4_1[6,8],sep=" - ")
todayeveningwind = paste(p4_1[7,14],p4_1[6,14],sep=" - ")
todayovernightwind = paste(p4_1[7,17],p4_1[6,17],sep=" - ")
tomorrowmorningwind = paste(p4_2[7,2],p4_2[6,2],sep=" - ")
tomorrowafternoonwind = paste(p4_2[7,8],p4_2[6,8],sep=" - ")

# Website 4

page4<-read_html("https://forecast.weather.gov/product.php?site=NWS&product=FWF&issuedby=PBZ")
table4<-page4 %>%
  html_nodes(".glossaryProduct") %>%
  html_text()
adiearly<-str_extract(table4,'ADI\\searly.{1,}')
adilate<-str_extract(table4,'ADI\\slate.{1,}')
adiearlysplit<-str_extract_all(adiearly,'\\d{1,2}\\s.{1,10}')
adiearlytoday<-trimws(adiearlysplit[[1]][1],whitespace=" ")
aetodvalue<-str_extract(adiearlytoday,'\\d{1,2}')
aetoddesc<-str_extract(adiearlytoday,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
adiearlytonight<-trimws(adiearlysplit[[1]][2],whitespace=" ")
aetonvalue<-str_extract(adiearlytonight,'\\d{1,2}')
aetondesc<-str_extract(adiearlytonight,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
adiearlytomorrow<-trimws(adiearlysplit[[1]][3],whitespace=" ")
aetomvalue<-str_extract(adiearlytomorrow,'\\d{1,2}')
aetomdesc<-str_extract(adiearlytomorrow,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))

# Late starts here
adilatesplit<-str_extract_all(adilate,'\\d{1,2}\\s.{1,10}')
adilatetoday<-trimws(adilatesplit[[1]][1],whitespace=" ")
altodvalue<-str_extract(adilatetoday,'\\d{1,2}')
altoddesc<-str_extract(adilatetoday,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
adilatetonight<-trimws(adilatesplit[[1]][2],whitespace=" ")
altonvalue<-str_extract(adilatetonight,'\\d{1,2}')
altondesc<-str_extract(adilatetonight,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
adilatetomorrow<-trimws(adilatesplit[[1]][3],whitespace=" ")
altomvalue<-str_extract(adilatetomorrow,'\\d{1,2}')
altomdesc<-str_extract(adilatetomorrow,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
if(aetoddesc=="Gen Poor"){
  aetoddesc<-"Generally Poor"
} else if (aetoddesc=="Gen Good"){
  aetoddesc<-"Generally Good"
}
if(altoddesc=="Gen Poor"){
  altoddesc<-"Generally Poor"
} else if (altoddesc=="Gen Good"){
  altoddesc<-"Generally Good"
}

if(aetondesc=="Gen Poor"){
  aetondesc<-"Generally Poor"
} else if (aetondesc=="Gen Good"){
  aetondesc<-"Generally Good"
}

if(altondesc=="Gen Poor"){
  altondesc<-"Generally Poor"
} else if (altondesc=="Gen Good"){
  altondesc<-"Generally Good"
}

if(aetomdesc=="Gen Poor"){
  aetomdesc<-"Generally Poor"
} else if (aetomdesc=="Gen Good"){
  aetomdesc<-"Generally Good"
}

if(altomdesc=="Gen Poor"){
  altomdesc<-"Generally Poor"
} else if (altomdesc=="Gen Good"){
  altomdesc<-"Generally Good"
}

todaymorning<-paste(aetoddesc,"-",aetodvalue)
todayafternoon<-paste(altoddesc,"-",altodvalue)
tonightevening<-paste(aetondesc,"-",aetonvalue)
tonightovernight<-paste(altondesc,"-",altonvalue)
tomorrowmorning<-paste(aetomdesc,"-",aetomvalue)
tomorrowafternoon<-paste(altomdesc,"-",altomvalue)

# Website 5
# surface inversion report
## scrape the data from website

times<-strptime(Sys.Date(),"%Y-%m-%d")
y<-as.character(format(times,"%Y"))
m<-as.character(format(times,"%m"))
d<-as.character(format(times,"%d"))

y<-as.character(format(strptime(Sys.Date(),"%Y-%m-%d"),"%Y"))
m<-as.character(format(strptime(Sys.Date(),"%Y-%m-%d"),"%m"))
d<-as.character(format(strptime(Sys.Date(),"%Y-%m-%d"),"%d"))
link<-paste("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=",y,"&MONTH=",m,"&FROM=",d,"12&TO=",d,"12&STNM=72520",sep="")

page5<-read_html(link)
table5<-page5 %>%
  html_nodes("pre:nth-child(2)") %>%
  html_text()

fiveextract<-str_extract_all(table5,'\\n.{22}')
fivenum<-fiveextract[[1]][-c(1:4)]
digits<-trimws(substr(fivenum,3,22),which=c("left"))
digitssplit<-str_extract_all(digits,'.{1}\\d{1,}.{1}\\d|\\d{1,}.{1}\\d')
pressure<-lapply(digitssplit,`[[`,1)
height<-lapply(digitssplit,`[[`,2)
digitssplit[[1]][3]<-""
temperature<-lapply(digitssplit,`[[`,3)
five<-cbind(as.numeric(pressure),as.numeric(height),as.numeric(temperature))
five<-five[-1,]
colnames(five)<-c("Pressure (hPa)","Height (m)","Temperature (C)")
tempdiff<-diff(five[,3])
surfaceinversion<-five[which(tempdiff<=0),3][1]-five[,3][1]
inversiondepth<-five[which(tempdiff<=0),2][1]-five[,2][1]

sentence<-five[which(five[,2]<1000),]
tempdiffunder1k<-diff(sentence[,3])
e<-which(tempdiffunder1k<0)
f<-diff(e)
g<-which(f>1)
upperinversion<-function(){
  if (is.na(tempdiffunder1k[e[g[1]+1]])){
    print("No upper inversion starting below ~1000 m is reported")
  } else print("Yes, an upper inversion starting below ~1000 m is reported")
}

fivestrength<-function(x){
  if (x==0){
    print("None")
  } else if (x>0 & x<1){
    print("Slight")
  } else if (x>=1 & x<3){
    print("Weak")
  } else if (x>=3 & x<5){
    print("Moderate")
  } else print("Strong")
}

## extract the variables for ADI
temp5 <- paste(round(surfaceinversion,1),"°C")
depth5 <- paste((inversiondepth),"m")
time5 <- "9am" 
scale5 <- fivestrength(surfaceinversion)
inversion5 <- upperinversion()

# Website 6

link6<-paste("https://rucsoundings.noaa.gov/get_soundings.cgi?data_source=GFS&start_year=",y,"&start_month_name=",month.abb[as.numeric(m)],"&start_mday=",as.numeric(d)+1,"&start_hour=12&start_min=0&n_hrs=1&fcst_len=shortest&airport=PIT&text=Ascii%20text%20%28GSL%20format%29&hydrometeors=false&startSecs=",as.numeric(as.POSIXct(Sys.Date()+1))+30000,"&endSecs=",as.numeric(as.POSIXct(Sys.Date()+1))+33600,sep="")

page6<-read_html(link6)
table6<-page6 %>%
  html_nodes("p") %>%
  html_text()

sixextracttoprow<-str_extract_all(table6,'9\\s{2}10000.{1,}')
sixremainingrows<-str_extract_all(table6,'4\\s{1,}.{1,}')
sixremainingrows<-sixremainingrows[[1]][-1]
unsplitsix<-c(sixextracttoprow,sixremainingrows)
splitsix<-str_extract_all(unsplitsix,'-\\d{1,}|\\d{1,}')
type<-lapply(splitsix,`[[`,1)
pressure<-lapply(splitsix,`[[`,2)
height<-lapply(splitsix,`[[`,3)
temp<-lapply(splitsix,`[[`,4)
dewpoint<-lapply(splitsix,`[[`,5)
winddirection<-lapply(splitsix,`[[`,6)
windspeed<-lapply(splitsix,`[[`,7)
six<-as.data.frame(cbind(type,as.numeric(pressure)/10,height,as.numeric(temp)/10,as.numeric(dewpoint)/10,winddirection,windspeed))
colnames(six)<-c("Type","Pressure (mb)","Height (m)","Temperature (C)","Dew Point (C)","Wind Direction (Degrees)","Wind Speed (Knots)")
tempdiffsix<-diff(unlist(six[,4]))[-1]
surfaceinversion2<-six[which(tempdiffsix<=0),4][[1]]-unlist(six[,4])[1]
inversiondepth2<-as.numeric(six[which(tempdiff<=0),3][[1]])-as.numeric(unlist(six[,3])[1])

sixout<-function(x){
  if (x==0){
    print("None")
  } else if (x>0 & x<1){
    print("Slight")
  } else if (x>=1 & x<3){
    print("Weak")
  } else if (x>=3 & x<5){
    print("Moderate")
  } else print("Strong")
}

# current date for report
currentDate <- Sys.Date()
title <- paste("Air Quality Forecast and Dispersion Outlook of Allegheny County, Pennsylvania for", as.character(currentDate))

# AQI
aqi <- data.frame(
  "Forecast Period"= c("Today","Tomorrow"),
  "Pittsburgh Area"=c(todaypitt,tomorrowpitt),
  "Liberty-Clairton Area"=c(todayLC,tomorrowLC))

aqi_forecast <- todayforecast[1]

# ADI
adi <- data.frame(
  "Forecast Period" = c("Today Morning","Today Afternoon", "Tonight Evening", 
                        "Tonight Overnight", "Tomorrow Morning", "Tomorrow Afternoon"),
  "Atmospheric Dispersion Index" = c(todaymorning,todayafternoon,tonightevening,tonightovernight,tomorrowmorning,tomorrowafternoon),
  "Surface Inversion Strength" = c(fivestrength(surfaceinversion),"--","--","--",sixout(surfaceinversion2),"--"),
  "Wind(dir,mph)"=c(todaymorningwind,todayafternoonwind,todayeveningwind,todayovernightwind,tomorrowmorningwind,tomorrowafternoonwind))

output = c(aqi,aqi_forecast,adi,temp5,depth5,time5,scale5,inversion5,title)



return(output)





}
  
  
  
x=scrape()

save(x, file = paste0("data-raw/data_", make.names(Sys.time()), ".Rda"))
