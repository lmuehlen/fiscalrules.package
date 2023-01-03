get_vis_data<-function(mod,mod_char){

  if(class(mod[[1]])[1]=="pgmm"){
    m<-lapply(mod,
              function(x){
                get_estimates(x,vcov = function(sd){plm::vcovHC(sd,cluster="countrycode")})
              })
  }else{
    m<-lapply(mod,
              function(x){get_estimates(x,draw=FALSE,cluster = ~countrycode)
    })
  }
  m<-do.call("rbind",m)

  m%>%
    tidyr::extract(term,into="term",regex = ".*(\\d+).*")%>%
    filter(!is.na(term))%>%
    mutate(term=paste0("Lag: ",term))


  m<-m%>%mutate(
    dep_var=gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\1",mod_char),
    level=gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\2",mod_char),
    model=gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\4",mod_char),
    dynlag=case_when(
      gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\5",mod_char)=="dynlag"~"upper bound",
      TRUE~"lower bound")
  )%>%dplyr::filter(grepl("Lag",m$term))%>%
    select(term,estimate,conf.low,conf.high,dep_var,level,model,dynlag)

  m$model<-plyr::revalue(m$model,c("within"="Within","iv"="IV","ab"="Arellano-Bond"))
  m$level<-plyr::revalue(m$level,c("sng"="Subnational gov.","lg"="local gov.","rg"="regional gov."))
  m$dep_var<-plyr::revalue(m$dep_var,c("gfcf"="Investment","gva"="Expenditure"))

  return(m)
}


