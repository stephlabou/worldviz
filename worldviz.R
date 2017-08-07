# ==========
# = Clear workspace =
rm(list = ls()) 
graphics.off()

# ==========
# = Set WD as needed by your machine!! 
base_dir<-getwd()
setwd(base_dir)

# ==================
# = Load Libraries
#function for installing packages first time only
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

#packages needed
packagesCRAN<-c("plyr","dplyr","devtools","reshape","gridExtra","hexbin","tidyr","rpart","rpart.plot",
                "party","partykit","dggridR","rgdal","ggthemes","RColorBrewer","scales","tweenr",
                "tidyverse","forcats")

#iterate usePackage function across package names
lapply(packagesCRAN,FUN=usePackage)

#additional finicky packages, handle separately

#install_github("tidyverse/ggplot2",force=TRUE)
library("ggplot2")

#install_github("dkahle/ggmap", force=TRUE)
library("ggmap")

#install_github("hrbrmstr/ggalt", force=TRUE)
library("ggalt")

#install_github("dgrtwo/gganimate", force=TRUE)
library("gganimate")

national_fert<-read.csv("Inputs_Fertilizers_E_All_Data_(Normalized).csv")
#national_fert_prior<-read.csv("Resources_FertilizersArchive_E_All_Data.csv")
country_codes<-read.csv("countries_codes_and_coordinates.csv",stringsAsFactors=FALSE)

names(national_fert)

##############################

country_codes$Country[which(country_codes$Country=="United States")]<-"United States of America"

fert<-merge(national_fert,country_codes,by.x="Area",by.y="Country", all=TRUE)



##############################

fertexport<-subset(fert,as.character(fert$Item.Code)=="3103" & fert$Element.Code %in% c(5915)) %>% group_by(code3=Alpha.3.code,area.code=as.character(Area.Code),area=as.character(Area),Year,Unit) %>%
  dplyr::summarize(fertexport_val=mean(Value,na.rm=TRUE)) %>% as.data.frame()
fertimport<-subset(fert,as.character(fert$Item.Code)=="3103" & fert$Element.Code %in% c(5615)) %>% group_by(code3=Alpha.3.code,area.code=as.character(Area.Code),area=as.character(Area),Year,Unit) %>%
  dplyr::summarize(fertimport_val=mean(Value,na.rm=TRUE)) %>% as.data.frame()
fertconsump<-subset(fert,as.character(fert$Item.Code)=="3103" & fert$Element.Code %in% c(5155)) %>% group_by(code3=Alpha.3.code,area.code=as.character(Area.Code),area=as.character(Area),Year,Unit) %>%
  dplyr::summarize(fertconsump_val=mean(Value,na.rm=TRUE)) %>% as.data.frame()

fert1<-merge(fertconsump,fertimport,by=c("code3","area.code","area","Year","Unit"),all=TRUE)
fert2<-merge(fert1,fertexport,by=c("code3","area.code","area","Year","Unit"),all=TRUE)
fertall<-fert2
fertall$netimport<-fertall$fertimport_val-fertall$fertexport_val 
fertall$importratio<-fertall$netimport/fertall$fertconsump_val
fertall$importratio[which(fertall$importratio==Inf)]<-NA
fertall$importratio[which(fertall$importratio==-Inf)]<-NA

yearcounts<-fertall %>% group_by(area) %>%
  dplyr::summarize(length(fertconsump_val)) %>% as.data.frame()

yearcounts.keep<-yearcounts[which(yearcounts[,2]==13),]

fertall<-subset(fertall,fertall$area %in% yearcounts.keep[,1])

fertall$colour<-"forestgreen"

fertall.ranks <- transform(fertall, 
                       consump_rank = ave(fertconsump_val, Year, 
                                          FUN = function(x) rank(-x, ties.method = "first")))
#                       importratio_rank = ave(importratio, Year, 
#                                          FUN = function(x) rank(-x, ties.method = "first")))

#fertall.ranksx<-subset(fertall.ranks,fertall.ranks$Year==2013) 
fertall.ranks<-subset(fertall.ranks,fertall.ranks$consump_rank<=100)

fertall.ranks<-transform(fertall.ranks,
                         consump_rank = ave(fertconsump_val, Year, 
                                            FUN = function(x) rank(x, ties.method = "first")),
                         importratio_rank = ave(importratio, Year, 
                                          FUN = function(x) rank(x, ties.method = "first")),
                       consump_rel = ave(fertconsump_val, Year, 
                                          FUN = function(x) rank(x, ties.method = "first")),
                       importratio_rel = ave(importratio, Year, 
                                          FUN = function(x) rank(x, ties.method = "first")))


####################################
fertall.ranks<-fertall.ranks[order(fertall.ranks$area,fertall.ranks$Year),]
####################################
datax <- data.frame(
  x = fertall.ranks$consump_rank,
  y = fertall.ranks$importratio_rank,
  z=fertall.ranks$Year,
  colour = 'forestgreen',
#  Year=fertall.ranks$Year,
  txt=as.character(fertall.ranks$code3)#,
#  stringsAsFactors = FALSE
)

datax.2002<-subset(datax,datax$z==2002)
datax.2003<-subset(datax,datax$z==2003)
datax.2004<-subset(datax,datax$z==2004)
datax.2005<-subset(datax,datax$z==2005)
datax.2006<-subset(datax,datax$z==2006)
datax.2007<-subset(datax,datax$z==2007)
datax.2008<-subset(datax,datax$z==2008)
datax.2009<-subset(datax,datax$z==2009)
datax.2010<-subset(datax,datax$z==2010)
datax.2011<-subset(datax,datax$z==2011)
datax.2012<-subset(datax,datax$z==2012)
datax.2013<-subset(datax,datax$z==2013)

tweendata <- tween_states(list(datax.2002, datax.2003, datax.2004, datax.2005,
                               datax.2006, datax.2007, datax.2008, datax.2009,
                               datax.2010, datax.2011, datax.2012, datax.2013,
                               datax.2002), 4, 1, ease=rep('cubic-in-out',12), 100)
tweendata <- tween_states(list(datax.2002, datax.2003, datax.2004, datax.2005,
                               datax.2006, datax.2007, datax.2008, datax.2009,
                               datax.2010, datax.2011, datax.2012, datax.2013), 2, 3, ease='cubic-in-out', 100)

tweendata$txt<-as.character(tweendata$txt)

tweendata<-tweendata[order(tweendata$txt,tweendata$z),]

# checking china- Why does it pulse in between years that have very similar x and y values? 
# what we want is a smoother interpolation using tween_state().
# on 7 Aug 2017 SP tried numerous "easing functions" (e.g., cubic-in-out, quadratic-in, etc.) and none product the desired behavior (?)
# remains unresolved
subset(tweendata,tweendata$txt==" CHN" & tweendata$z>=2003 & tweendata$z<=2004)

p <- ggplot(tweendata, aes(x = x, y = y, z=z)) +
  geom_text(aes(frame=z,label=txt))+
#  ggtitle(tweendata$z)+
  xlim(0,103)+ylim(0,103)+
  xlab("P consumption")+ylab("P import ratio")

animation::ani.options(interval = 1 / 6)
gganimate(p)#, title_frame = FALSE)



#######################

do<-0

if(do==1){

datax <- data.frame(
  x = log10(fertall.ranks$fertconsump_val),
  y = log10(fertall.ranks$fertimport_val),
  z=fertall.ranks$Year,
  colour = 'forestgreen',
  #  Year=fertall.ranks$Year,
  txt=fertall.ranks$code3#,
#  stringsAsFactors = FALSE
)
datax.2002<-subset(datax,datax$z==2002)
datax.2003<-subset(datax,datax$z==2003)
datax.2004<-subset(datax,datax$z==2004)
datax.2005<-subset(datax,datax$z==2005)
datax.2006<-subset(datax,datax$z==2006)
datax.2007<-subset(datax,datax$z==2007)
datax.2008<-subset(datax,datax$z==2008)
datax.2009<-subset(datax,datax$z==2009)
datax.2010<-subset(datax,datax$z==2010)
datax.2011<-subset(datax,datax$z==2011)
datax.2012<-subset(datax,datax$z==2012)
datax.2013<-subset(datax,datax$z==2013)

tweendata <- tween_states(list(datax.2002, datax.2003, datax.2004, datax.2005,
                               datax.2006, datax.2007, datax.2008, datax.2009,
                               datax.2010, datax.2011, datax.2012, datax.2013,
                               datax.2002), 1, 1, 'bounce-in', 100)
#tweendata$Year4<-substr(tweendata$Year,1,4)

p <- ggplot(tweendata, aes(x = x, y = y, z=z)) +
  geom_text(aes(frame=z,label=txt))+
  #  ggtitle(tweendata$z)+
# xlim(0,103)+ylim(0,103)+
  xlab("P consumption")+ylab("P import ratio")

animation::ani.options(interval = 1 / 2)
gganimate(p)#, title_frame = FALSE)

#end do loop
}

#######################
