library(XML)
#Define function for getting kicking data
get.x<-function(date,team){
	url<-paste0("http://www.pro-football-reference.com/boxscores/",date,"0",team,".htm")

x<-readHTMLTable(url,header=FALSE)$pbp_data

x<-x[x$V1!="",]

x<-as.character(x$V6)

x<-x[!is.na(x)]	

out<-x[unlist(gregexpr("field goal",x))>0]
out}

#get.x('20120930',"atl")

#Getting the data
month.hash<-list()
#month.hash[['08']]<-31
month.hash[['09']]<-30
month.hash[['10']]<-31
month.hash[['11']]<-30
month.hash[['12']]<-31
month.hash[['01']]<-31
month.hash[['02']]<-29

#kick.list<-list()
year<-"2012"
t.vec<-c("gnb","phi","htx","nor","crd","den","cle","min","tam","chi","nyj","oti","rav","rai","nyg","ram","was","buf","kan","clt","car","sea","mia","sfo","sdg","dal","pit","nwe","jax","atl","det","cin")
	for (yearmonth in c(paste0(year,c("09","10","11","12")),paste0(as.character((as.numeric(year)+1)),c("01","02")))){
			#for (yearmonth in c("201109","201110","201111","201112","201201","201202")){
	month<-substring(yearmonth,5,6)
for (day in c(paste0("0",c(1:9)),as.character(c(10:month.hash[[month]])))){
	for (t in t.vec){
		d<-paste0(yearmonth,day)
			print(c(year,yearmonth,day,t))
			tmp<-try(get.x(d,t))
			print(tmp)
			if (class(tmp)!="try-error"){kick.list[[year]][[paste0(d,t)]]<-tmp}
				}
				}}
				
		
make.kick.df<-function(year){				
kick.vec<-c(unlist(kick.list[[year]]))
#remove '(no play)'
kick.vec<-kick.vec[unlist(gregexpr('(no play)',kick.vec))<0]

#pull out yardage
yards.index<-unlist(lapply(gregexpr('[0-9]',kick.vec),min))
yards<-substring(kick.vec,yards.index,yards.index+1)

#Pull out kicker name
name<-gsub(" ","",substring(kick.vec,1,yards.index-2))

#Did they make it?
good<-yards
good<-0
good<-(unlist(gregexpr("field goal good",kick.vec))>0)+0


out<-data.frame(name,yards,good,year=year)
out
}
kick.df<-list()
kick.df[['2012']]<-make.kick.df('2012')
kick.df[['2011']]<-make.kick.df('2011')
kick.df[['2010']]<-make.kick.df('2010')
kick.df[['2009']]<-make.kick.df('2009')
kick.df[['2008']]<-make.kick.df('2008')
kick.df[['2007']]<-make.kick.df('2007')
kick.df[['2006']]<-make.kick.df('2006')
kick.df[['2005']]<-make.kick.df('2005')
kick.df[['2004']]<-make.kick.df('2004')
kick.df[['2003']]<-make.kick.df('2003')
kick.df[['2002']]<-make.kick.df('2002')
kick.df[['2001']]<-make.kick.df('2001')
kick.df[['2000']]<-make.kick.df('2000')
#save.image("Kick_Database.RData")

				
kick.dat<-do.call(rbind,kick.df)	
kick.dat$yards<-as.numeric(as.character(kick.dat$yards))	
#write.csv(kick.dat,"kick_dat.csv")


