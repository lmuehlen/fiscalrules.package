get_vis_data<-function(mod,conf_level=0.95,cluster_name="nuts_id"){

name<-deparse(substitute(mod))
  if(class(mod[[1]])[1]=="pgmm"){
    m<-lapply(mod,
              function(x){
                a<-coefci(x,level=conf_level,vcov. = vcovHC(x))%>%#Windmejer adjusted SEs
                  as_tibble()%>%
                  select(conf.low=1,conf.high=2)
                b<-coeftest(x,vcov = vcovHC(x))
                get_estimates(b)%>%
                  mutate(
                    conf.low=a$conf.low,
                    conf.high=a$conf.high)
              })
  }else{
    m<-lapply(mod,
              function(x){
                get_estimates(x,conf_level = conf_level,draw=FALSE,vcov = vcov_cluster(x, cluster = cluster_name, ssc = NULL))
              })
  }
  m<-do.call("rbind",m)

  m<-m%>%
    filter(grepl("frd",term))%>%
    mutate(
      rule_type=gsub(".*(full|bbr|dr|er|rr).*","\\1",term),
      level=gsub(".*(lg|rg|sng).*","\\1",term)
    )%>%
    tidyr::extract(term,into="term",regex = ".*(\\d+).*")%>%
    mutate(
      term=case_when(!is.na(term)~paste0("Lag: ",term),
                     TRUE~"Lag: 0"
      )
    )

  m<-m%>%mutate(
    dep_var=gsub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\1",name),
    model=gsub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\5",name),
    upper_lower=case_when(
      gsub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\6",name)=="upperbound"~"Upper bound",
      TRUE~"Lower bound")
  )%>%dplyr::filter(grepl("Lag",m$term))%>%
    select(term,estimate,conf.low,conf.high,dep_var,level,model,upper_lower,rule_type)


  m$model<-plyr::revalue(m$model,c("within"="Within","iv"="IV","ab"="Arellano-Bond"))
  m$rule_type<-plyr::revalue(m$rule_type,c("bbr"="Budget balance rule","er"="Expenditure rule","dr"="Debt rule"))
  m$level<-plyr::revalue(m$level,c("sng"="Subnational gov.","lg"="local gov.","rg"="regional gov."))
  m$dep_var<-plyr::revalue(m$dep_var,c("gfcf"="Investment","gva"="Expenditure","ratio"="Ratio"))

  return(m)
}


