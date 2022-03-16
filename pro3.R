library(mapview)
library(tigris)
library(tidycensus)
library(dplyr)
install.packages("curl")
install.packages(c("httr"))
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
remotes::install_github('walkerke/tigris')
library(sf)
library(curl)
library(httr)
library(leaflet)
library(leafpop)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(purrr)
options(xtable.include.rownames=T)
options(xtable.include.colnames=T)
  set.seed(5)
if (FALSE) {
  census_api_key("111111abc", install = TRUE)
  # First time, reload your environment so you can use the key without restarting R.
 
}
census_api_key("6b39cc5661b7cefd31443142a5e20fcd31fc72bb",install=TRUE,overwrite=TRUE)
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")

evls1<-read.table(file = "~/Documents/SP2021/CS424/work4/energy11(v3).csv", sep = ",",skipNul="TRUE",header = TRUE)

sapply(evls1,class)





csf4  <- subset(evls1,str_detect(com,",") ==TRUE)

length(unique(evls1$com))



#evls01 <- subset( evls,total_the==0 | total_kwh==0 )
#evls11  <- subset(evls1,is.na(the_3) |is.na(the_4) | is.na(the_5) |is.na(the_6)| is.na(the_7) | is.na(the_8) | is.na(the_9)| is.na(the_10) | is.na(the_11) |  is.na(the_12) )
#evls13  <- subset(evls1,is.na(kwh_3) |is.na(kwh_4) | is.na(kwh_5) |is.na(kwh_6)| is.na(kwh_7) | is.na(kwh_8) | is.na(kwh_9)| is.na(kwh_10)) 

#evls1$census_blocks  <- substr(evls1$census_blocks,)
evls1$census_blocks <-substring(evls1$census_blocks,2)
evls1$com  <- ifelse(str_detect(evls1$com,","),"Ohare",evls1$com)

evls1$kwh_1 <- ifelse(is.na(evls1$kwh_1),0,evls1$kwh_1) 
evls1$kwh_2 <- ifelse(is.na(evls1$kwh_2),0,evls1$kwh_2) 
evls1$kwh_3 <- ifelse(is.na(evls1$kwh_3),0,evls1$kwh_3) 
evls1$kwh_4 <- ifelse(is.na(evls1$kwh_4),0,evls1$kwh_4) 
evls1$kwh_5 <- ifelse(is.na(evls1$kwh_5),0,evls1$kwh_5) 
evls1$kwh_6 <- ifelse(is.na(evls1$kwh_6),0,evls1$kwh_6) 
evls1$kwh_7 <- ifelse(is.na(evls1$kwh_7),0,evls1$kwh_7) 
evls1$kwh_8 <- ifelse(is.na(evls1$kwh_8),0,evls1$kwh_8) 
evls1$kwh_9 <- ifelse(is.na(evls1$kwh_9),0,evls1$kwh_9) 
evls1$kwh_10 <- ifelse(is.na(evls1$kwh_10),0,evls1$kwh_10) 
evls1$kwh_11 <- ifelse(is.na(evls1$kwh_11),0,evls1$kwh_11) 
evls1$kwh_12<- ifelse(is.na(evls1$kwh_12),0,evls1$kwh_12) 
evls1$total_kwh <-  ifelse(is.na(evls1$total_kwh),0,evls1$total_kwh) 
evls1 <- subset(evls1, !(is.na(GEOID)) & building_type !="")


evls1$the_1 <- ifelse(is.na(evls1$the_1),0,evls1$the_1) 
evls1$the_2 <- ifelse(is.na(evls1$the_2),0,evls1$the_2) 
evls1$the_3 <- ifelse(is.na(evls1$the_3),0,evls1$the_3) 
evls1$the_4 <- ifelse(is.na(evls1$the_4),0,evls1$the_4) 
evls1$the_5 <- ifelse(is.na(evls1$the_5),0,evls1$the_5) 
evls1$the_6 <- ifelse(is.na(evls1$the_6),0,evls1$the_6) 
evls1$the_7 <- ifelse(is.na(evls1$the_7),0,evls1$the_7) 
evls1$the_8 <- ifelse(is.na(evls1$the_8),0,evls1$the_8) 
evls1$the_9 <- ifelse(is.na(evls1$the_9),0,evls1$the_9) 
evls1$the_10 <- ifelse(is.na(evls1$the_10),0,evls1$the_10) 
evls1$the_11 <- ifelse(is.na(evls1$the_11),0,evls1$the_11) 
evls1$the_12<- ifelse(is.na(evls1$the_12),0,evls1$the_12) 
evls1$total_the <-  ifelse(is.na(evls1$total_the),0,evls1$total_the) 

names(evls1)[names(evls1)=="census_blocks"]  <- "GEOID"


evls029 <- evls1 %>% group_by(GEOID) %>% summarise_at(vars(total_kwh),sum)
evls02 <- subset(evls1,is.na(total_units) |  is.na(total_population) | is.na(stories) | is.na(building_age) | is.na(house_size) )
evls022 <- subset(evls1, total_the ==0 )
evls023 <- subset(evls1,is.na(re_oc_percent) | is.na(oc_unit_percent) )
evls024 <- subset(evls1,oc_unit_percent ==0)
evls025 <- subset(evls1, is.na(oc_unit_percent)  )
evls026  <- subset(evls1, is.na(re_oc_percent) )
evls03  <-    subset(evls1,  is.na(total_population) | is.na(stories) | is.na(building_age) |is.na(building_type) ) 

evls022

smallcom <- subset(evls1,com =="Loop")
smallcom <- subset(evls1,str_detect(com,"Near"))




cs<-unique(evls1$com)

csf <- data.frame(matrix(unlist(cs), nrow=length(cs), byrow=TRUE))
csf1 <- subset(evls1, nchar(com) > 15)



ab <- get_acs( geography="block group", state ="IL",variables = "B01003_001" , county="Cook",geometry=TRUE)

aa <-get_decennial(geography = "block", state ="IL", variables = "P001001", county="Cook",geometry = TRUE)
sapply(aa,class)

#evls12<-  evls1 %>% rename(GEOID=census_blocks) # %>% rename(estimate =total_kwh) %>% rename(NAME=com) %>% select(GEOID,NAME,estimate)
#evls10  <- aa %>%inner_join(evls1,"GEOID") 
evls11 <-  evls1 %>%  select(GEOID,com,total_kwh,building_type)   

evls11_uni <-  evls1  %>% group_by(GEOID)  %>% summarise(n=n()) 

evls11 <- group_by(evls11,GEOID) %>% summarise_at(vars("total_kwh"),sum ) 

#evls16<-subset(evls1,building_type=="Commercial")
#subset(evls16,GEOID=="170310101001003") >
#evls16_uni <-  evls16  %>% group_by(GEOID)  %>% summarise(n=n()) 
#evls027<-subset(evls16,is.na(com))
evls16 <- evls1 %>% group_by(GEOID) %>% summarise(total_kwh=sum(total_kwh),com= my_all_com(com))
evls15 <- evls1 %>% group_by(GEOID) %>% summarise(type_build = type_building(building_type)) 

evls151 <-subset(evls15,str_detect(type_build,"Industrial")== TRUE)
evls031  <-subset(evls1,evls1$building_type =="")
evls161 <- subset(evls16, str_detect(com,",")== TRUE)

evls21  <- subset(evls1, com == "Near West Side") 
evls21 <- evls21 %>% group_by(GEOID) %>% summarise(total_kwh=sum(total_kwh),com= my_all_com(com))


#aa3  <- aa %>% left_join(evls11,"GEOID")   
#aa3 <- merge.data.frame(aa,evls11,by.x="GEOID",by.y="GEOID") 
aa3 <-  aa %>% inner_join(evls11,"GEOID")  
aa4  <- aa %>% inner_join(evls21,"GEOID") 


#evls11$total_kwh<-ifelse(is.na(evls11$total_kwh),0,evls11$total_kwh)

aa2 <- aa
rea <- rep(1:5,each=nrow(aa)/5) 
aa2$sam <- sample(1:5,nrow(aa2),replace =TRUE)



# evls11 <-group_by(evls11,GEOID) %>% summarise_at(vars("total_kwh"),sum ) 
#evls11  <-evls11  %>% left_join(aa,"GEOID","GEOID")  %>% select(GEOID,NAME,total_kwh,variable,value,geometry)  %>% filter(GEOID > 0)
evls112 <-subset(evls11,total_kwh==0) 
sapply(evls11,class)
aa1 <-subset(aa,GEOID=="170318030121020")
aa1 <-select(aa1,-c("geometry","value"))






co1 <-c("#34eb6e","#ebd334","#eb8934","#eb6834","#d034eb")
co2  <-c("#34eb6e","#ebd334","#f0d573","#ed9261","#eb8934","#eb6834","#ae8aeb","#d034eb")
co3  <- c( "#9ecae1","#afccfa","#6eabf5","#3182bd","#b85aae","#8877ed","#756bb1")
co4 <- c("#5ab4ac","#15bfbf","#aff4fa","#c7afa5","#c99e89","#bf8265","#c2b85d","#ada76c","#a8874d")
brewer.pal(8, "Dark2")
brewer.pal(8,"Pastel1")
au_map  <- mapview(aa4,zcol="total_kwh",col.regions =co3,layer.name = 'Year' ) 
au_map



#brewer.pal(4, "Dark2")

pal <- colorNumeric( palette = "YlGnBu",domain =evls12$total_kwh  ) 
mypopup <- paste0("GEOID: ", evls12$GEOID, "<br>", "Total_kwh: ", round(evls12$total_kwh,0))


mapview(ab)



leaflet() %>% addProviderTiles(providers$Stamen.TonerLite) %>%  addPolygons(data = ab,fill = FALSE) 
  
addLegend(pal = pal, values = ~total_kwh,  position = "bottomright", title = "Kilo") %>% 
  setView(lng = -88.825530475, lat = 40.51431925,zoom = 12)
 



type_building <-function(x){
  cur <-c()
  
  for(da in x){
    
    if(!(da %in% cur)){
        same <- TRUE
        cur <- c(cur,da)
     
    }
  }
  if(length(cur)>1){
    c<-""
    e <- 1
    for(a in cur){
    
      if(e==length(cur)){
        c  <- paste(c,a,sep="")
       
      }
      else{
        c  <- paste(c,a,", ",sep="") 
        e<-e+1
      }
    
    }
    return(c)
  }
  else{
    return(cur[1])
  }
  
}

my_random  <- function(x){
  index <- round(runif(1,min=0,max=length(x)),0)
 
  return(x[index[1]])
}


my_all_com <- function(x){
  uni <-c()
  wordy <- ""
  co<- 1
  if(length(x)==1){
    return(x[1])
  }
  else{
    
    for(i in x){
      if(!(i %in% uni)){
        uni <-c(uni,i)
        
      }
    }
   
    for(i in uni){
      if( co ==1){
        wordy <- i 
      }
      else{
 
       wordy <- paste(wordy,i,sep=",")
       
      }
      co <- co +1
       
    }
   
    return(wordy)
    
  }
}












suppressMessages(tracts <- tracts(state = 'NY', county=c(5, 47, 61, 81, 85), cb=TRUE))

vars10 <- c("P005003", "P005004", "P005006", "P004003")

il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P001001", state = "IL", geometry = TRUE) %>%
  mutate(pct = 100 * (value / summary_value))
mapview(il)
ggplot(il, aes(fill = pct, color = pct)) +
  geom_sf() +
  facet_wrap(~variable)




lookup_code(state="IL",county="Cook")
cc<-tracts(state='17', county='031')
tracts <- tracts(state = 'NY', county=c(5, 47, 61, 81, 85), cb=TRUE)


geo.make(state="IL", county =c(031),tract = "*" )

vars <- paste("P003000", 1:8,sep="", collapse = ",")

de<- data.frame(c("1","2","1"),c("19","10","11"))
colnames(de)  <- c("xi","zi")
de 
de2<- data.frame(c("1","2","1"),c("5","6","3"),c("1","1","2"))
colnames(de2)  <- c("gi","zi","xi")
de2
a<-merge.data.frame(de,de2,by.x="xi", by.y= "gi")
a
de %>% left_join(de2,by = "xi",suffix = c(".xx",".yy"))
names(de)
substring("as",2)

select(de,c("zi","xi"))
filter(de, zi >11)


eo <-function(x){
  length(x)+100
}
ei <- function(x,f){
  print(f)
  if(x[1]==1){
    df <- data.frame(xi=c(1,2),yi=x)
  }
  else{
    df <-  data.frame(xi=c(1,1,2,2),yi=rep(1:4,1))
  }
  return(df)  
}

look_list <- list(mean = c("x", "y"),
                  median = "z")

de4  <-data.frame(c(1,1,1,2,2,2,2),c(4,4,5,1,10,10,10),c("a","a","c","c","c","i","q"))
colnames(de4)  <- c("xi","yi","zi")
de4 %>% group_by(xi)  %>% summarise_at(vars("yi","zi"),list(mysum=sum,n=n())) %>%  distinct(zi, .keep_all=TRUE)  

de4 %>% group_by(xi,yi)  %>% summarise(sum =eo(xi))
de4 %>% group_by(zi) %>%summarise_at(vars("xi","yi"),funs((eo)(.)) )
de4 %>% group_by(zi)  %>% summarise_at(vars("yi","xi"),list(mysum =sum,mymean= mean))

names(look_list)
look_list("mean")

  l <- list(c(1,2), c(2,3))
  
 l2<- map2(l,c(mean,sum) ,ei) 
 l2
 l3 <-reduce(l2,left_join,by="xi"  )
 l3
 aa[]
l[1]

su1  <- function(a,f){
  print("ddd")
  f(a)
  
}
plus100 <- function(x,y,f){
   x+100
}
plus101 <- function(x,y){
  x+100
}
su2  <- function(a,b){
   a-b
}

su3  <- function(a,b,f){
  print("aaa")
  a+b
 
}

au1 <- list(c(1,2),c(2,2))

au2  <- list(a =1,b=2)
au3 <- list(r=2,e= 4)

aut <-map2(au1,c(sum,sum),su1) 

map2_dbl(au2,au3,sum)

pmap_dbl(list(au2,au3),su2)
pmap_dbl(list(au2,au3),c(plus101),su3)








de4  %>% group_by(xi,yi)  %>% mutate(a=eo(zi))  %>% group_by(xi,yi,zi ) %>% filter(zi == min(zi)) 


  a <-c(1,3,4)
  
  length(a)
  for(x in seq(1,length(a),by=1)){
    print(x)
  }
  
  for(x in a){
    print(x)
  }

c<-round(runif(1,min=1,max=2),0)
c[2]

str_detect("a, boio,", "b")

word1 <- paste("a","b i",sep=",",collapse ="-")
    
    
da <- data.frame(a=c(1,2,3),b=c(1,7,8))
sapply(da,class)
  
ggplot(da,aes(x=a,y=b))
co<-c(6,8,7)
df <- data.frame(matrix(unlist(co), nrow=length(co), byrow=TRUE))
df
names(df) <-"a"
df
sapply(df,class)
rep(1:4,2)

evls14 <-c(1,2,3,4,5,6)
evls15 <-data.table(matrix(unlist(evl14),nrow =2,byrow=TRUE))
colnames(evls15) <-c("1","2","3")
rownames(evls15) <- c("a","b")
evls15

x <-"si"

ay <- function(){
  if(x =="soi"){
    yio <-"hoi1"
  }
  else {
    yio<- "hoi2" 
  }
  assign(yio,3,envir=parent.frame())
}

r<-"atv" 
nchar(r)
c<-substring(r,length(r))
c
list(1,data.frame(x=c(1,2)) )

