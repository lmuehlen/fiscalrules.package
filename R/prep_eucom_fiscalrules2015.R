prep_eucom_fiscalrules2015<-function(inputfile,outputfile){
  ec_frd2015 <- read_excel(inputfile, sheet = 2)

  # for reshaping
  mec_frd2015 <- read_excel(inputfile, sheet = 3)

  mec_frd2015 <- mec_frd2015 %>%
    pivot_longer(-c(no, country, type, sector, cover), names_to = "year", values_to = "fr_indicator") %>%
    select(no, year, fr_indicator)

  mec_frd2015 <- mec_frd2015 %>% mutate(fr_indicator = case_when(
    is.na(fr_indicator) ~ 0,
    TRUE ~ 1
  ))

  ec_frd_merge2015 <- left_join(mec_frd2015, ec_frd2015, by = c("no" = "no")) %>%
    mutate(across(!c(year, Country) & where(is.character),
                  ~ case_when(
                    fr_indicator == 0 ~ NA_character_,
                    TRUE ~ .
                  ),
                  .names = "{.col}"
    )) %>%
    mutate(across(!c(year, Country) & where(is.numeric),
                  ~ case_when(
                    fr_indicator == 0 ~ NA_real_,
                    TRUE ~ .
                  ),
                  .names = "{.col}"
    )) %>%
    rename_with(~ gsub("\\s-.*", "", .x), matches("^C\\d")) # Renames for example 'C1 - Satuatory base' to C1

  # this step is necessary since the coding of the dataset is assingning both ER and BBR at the same time to one rule only for Italy between 2009 and 2015. I create two rules
  ec_frd_merge2015 <- rbind(
    ec_frd_merge2015 %>%
      mutate(Type = case_when(
        Type == "ER/BBR" ~ "ER",
        TRUE ~ Type
      )),
    ec_frd_merge2015 %>% filter(Type == "ER/BBR") %>% mutate(Type = "BBR")
  )

  ec_frd_merge2015 <- ec_frd_merge2015 %>%
    mutate(C2=case_when(C2==0~1,#Correcting coding error in ec database
                        TRUE~C2),
           C3 = case_when(
             C3a + C3c == 2 ~ 1, # no monitoring, no enforcement (2)
             C3a + C3c < 5 ~ 2, # monitoring and/or enforcement (not independent) (3,4)
             C3a + C3c > 4 ~ 3, # monitoring and enforcement (at least one independent) (5,6)
           ),# creating one value for C3 is necessary since the computation of the POSET would not be feasible otherwise
           C4=case_when(C4a>2~3, #values 2 and 4 after 2015 do not have the same definition as 3 and 4 before 2015 (although not too different), aggregation best solution
                        C4a==2~2,
                        C4a==1~1),
           Exclusions=case_when(Exclusions=="Y"~1,
                                TRUE~0))%>%
    select(no,countrycode=Country,year,type=Type,sector=Sector,cover=`Coverage of GG finances`,C1, C2, C3, C4,Exclusions)
  saveRDS(ec_frd_merge2015, outputfile)

return(ec_frd_merge2015)
}
