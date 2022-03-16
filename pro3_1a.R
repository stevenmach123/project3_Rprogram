aa31 <-get_acs(geography= "tract",state= "IL", variables = "B01003_001",county ="Cook", geometry=TRUE)

aa31  <- aa31  %>% select(-c("moe","estimate","variable","NAME"))

evls3 <- subset(evls1, !is.na(total_units) & !is.na(total_population) & !is.na(house_size) )
#evls3  <- subset(evls3, !is.na(oc_unit_percent) & oc_unit_percent != 0 & !is.na(re_oc_percent) )
#evls3  <- subset(evls3, !is.na(re_oc_percent) )
evls3$GEOID  <- substr(evls3$GEOID,1,11) 
evls3$oc_unit_percent <- ifelse(is.na(evls3$oc_unit_percent),0,evls3$oc_unit_percent )
evls3$re_oc_percent <- ifelseif(is.na(evls3$re_oc_percent),0,evls3$re_oc_percent ) 


evls31  <- evls3 %>% group_by(GEOID) %>% summarise_at(vars("total_kwh"),sum)
evls32 <-inner_join(aa31,evls31)

fl3 <-data.frame(x=c(""),y=c(""),z=c(""))
names(fl3)  <- c("GEOID","total_kwh","geometry") 
fl3 <-subset(fl3, GEOID !="") 
len <- as.integer(nrow(evls32) * 0.1)
for(i in seq(1,len,by=1)){
  nmax <-max(evls32$total_kwh)
  ar3 <- evls32[evls32$total_kwh == nmax, ]  
  fl3 <- rbind(fl3,ar3)
  evls32  <-subset(evls32,evls32$total_kwh  != nmax)
}
mapview(fl3,zcol="total_kwh")



e  <-c(1.2,3.1)
e  <- as.integer(e)
e

round(1.237,2)