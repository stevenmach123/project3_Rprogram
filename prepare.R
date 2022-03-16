aa <-get_decennial(geography = "block", state ="IL", variables = "P001001", county="Cook",geometry = TRUE)
aa31 <-get_acs(geography= "tract",state= "IL", variables = "B01003_001",county ="Cook", geometry=TRUE)
aa31  <- aa31  %>% select(-c("moe","estimate","variable","NAME"))


