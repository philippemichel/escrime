tabcompx <- function(dfx, tri, 
                      titre = "Tableau comparatif", 
                      lab = "tabcomp"){
  #On supprime les données manquantes dans la variable de tri
  #dfx <- dfx %>% 
  #filter(!is.na({{tri}}))
  #
  tabx <- c()
  trix <- enquo(tri)
  vv <- quo_name(trix)
  triz <- dfx[vv]
  triz <- triz[,1]
  for (ll in 1:length(dfx)){
    varx <- dfx[,ll]
    nom <- names(dfx)[ll]
    if (nom != vv){
        ttp <- t.test(varx~triz, paired = TRUE, var.equal = TRUE)
        ppx <- round(ttp$p.value, 3)
        #
        zz <- dfx %>% 
          group_by(tri) %>% 
          summarize(mm =mean(varx), ss = sd(varx))
        aav <- paste0(round(zz[1,2],2), " ± ", round(zz [1,3],2))
        aap <- paste0(round(zz[2,2],2), " ± ", round(zz [2,3],2))
        #
        lig <- c(aav,aap)
        tabx <- rbind(tabx,lig)
     
  }
  # Création tableaux
  ltit <- c(" ",levels(triz),"p")
    kable(tabx, 
          row.names = FALSE, 
          col.names = ltit, 
          caption = titre, 
          label = lab, 
          escape = FALSE)
  } %>% 
    kable_styling(bootstrap_options = "striped", full_width = FALSE,
                  position = "center")
}