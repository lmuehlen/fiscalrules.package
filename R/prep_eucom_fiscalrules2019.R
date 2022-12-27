prep_eucom_fiscalrules2019<-function(inputfile,outputfile){
  ec_frd2019 <- read_excel(inputfile, sheet = 2,skip=1)%>%
    rename_with(~ gsub("\\s-.*", "", .x), matches("^C\\d"))%>%
    select(no=`Rule code`,countrycode=Country,"C3c_base"=`Body in charge of establishing the existence of a deviation from the target`,C1,C2,C3a,C3b,C3c,C3d,C4,C5a,C5b,C5c,Exclusions=C5d)



  # for reshaping
  mec_frd2019 <- read_excel(inputfile, sheet = 3)

  mec_frd2019 <- mec_frd2019 %>%
    pivot_longer(-c(no, country, type, sector, cover), names_to = "year", values_to = "fr_indicator") %>%
    select(no,type,sector,cover, year, fr_indicator)

  mec_frd2019 <- mec_frd2019 %>% mutate(fr_indicator = case_when(
    is.na(fr_indicator) ~ 0,
    TRUE ~ 1
  ))


  ec_frd_merge2019 <- left_join(mec_frd2019, ec_frd2019, by = c("no" = "no")) %>%
    mutate(across(!c(year, countrycode) & where(is.character),
                  ~ case_when(
                    fr_indicator == 0 ~ NA_character_,
                    TRUE ~ .
                  ),
                  .names = "{.col}"
    )) %>%
    mutate(across(!c(year, countrycode) & where(is.numeric),
                  ~ case_when(
                    fr_indicator == 0 ~ NA_real_,
                    TRUE ~ .
                  ),
                  .names = "{.col}"
    ))%>%
    select(countrycode,year,everything())%>%
    mutate(across(matches("^C(.){1,2}$"),~as.numeric(.),.names = "{.col}"))




  ec_frd_merge2019<-ec_frd_merge2019%>%
    mutate(C1=C1+1,
           C2=case_when(C2==3~3,
                        C2==1~2,
                        C2==0~1),
           C3a=case_when(C3a>1~3,
                         C3a==1~2,
                         C3a==0~1),
           C3c=case_when(C3c>0~3,#using the definition used before 2015 (not a major difference since we treat them ordinally)
                         grepl("[Mm]inistry|[Gg]overnment",C3c_base)~2,
                         C3c==0~1),
           C3 = case_when(
             C3a + C3c == 2 ~ 1, # no monitoring, no enforcement (2)
             C3a + C3c < 5 ~ 2, # monitoring and/or enforcement (not independent) (3,4)
             C3a + C3c > 4 ~ 3, # monitoring and enforcement (at least one independent) (5,6)
           ),# creating one value for C3 is necessary since the computation of the POSET would not be feasible otherwise
           C4=case_when(C4>1~3, #values 2 and 4 after 2015 do not have the same definition as 3 and 4 before 2015 (although not too different), aggregation best solution
                        C4==1~2,
                        C4==0~1
                       ),
           Exclusions=case_when(Exclusions=="1"~1,
                                TRUE~0)
    )


  ec_frd_merge2019<-ec_frd_merge2019%>%
    select(no,countrycode,year,type,sector,cover,C1,C2,C3,C4,Exclusions)%>%
    filter(year>2015)
  saveRDS(ec_frd_merge2019,outputfile)

return(ec_frd_merge2019)
}
