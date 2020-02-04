

qq <- function(dfx,trix){
aa <- dfx %>% 
  group_by({{trix}}) %>% 
     map(ligx) 
#aa <- aa[-length(aa)]
return(aa)
}

aa <- iris %>% 
  group_by(Species)%>% 
  map(mean) 
#aa <- aa[-length(aa)]

