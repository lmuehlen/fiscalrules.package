get_vis_data<-function(mod){

name<-deparse(substitute(mod))
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

  m<-m%>%
    filter(grepl("frd",term))%>%
    tidyr::extract(term,into="term",regex = ".*(\\d+).*")%>%
    mutate(
      term=case_when(!is.na(term)~paste0("Lag: ",term),
                     TRUE~"Lag: 0"
      )
    )

  m<-m%>%mutate(
    dep_var=gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\1",name),
    rule_type=gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\2",name),
    level=gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\3",name),
    model=gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\5",name),
    dynlag=case_when(
      gsub("(.*)_(.*)_(.*)_(.*)_(.*)","\\6",name)=="dynlag"~"upper bound",
      TRUE~"lower bound")
  )%>%dplyr::filter(grepl("Lag",m$term))%>%
    select(term,estimate,conf.low,conf.high,dep_var,level,model,dynlag,rule_type)

  m$model<-plyr::revalue(m$model,c("within"="Within","iv"="IV","ab"="Arellano-Bond"))
  m$level<-plyr::revalue(m$level,c("sng"="Subnational gov.","lg"="local gov.","rg"="regional gov."))
  m$dep_var<-plyr::revalue(m$dep_var,c("gfcf"="Investment","gva"="Expenditure"))

  return(m)
}


