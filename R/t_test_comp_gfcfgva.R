t_test_comp_gfcfgva<-function(gva,gfcf,cluster="nuts_id"){

  if(class(gfcf[[1]])[1]=="fixest"){
    gfcf_base<-gfcf %>%map_df(function(x){
      get_estimates(x,vcov=vcov_cluster(x, cluster = cluster, ssc = NULL))%>%filter(grepl("frd",term))
    })%>%select(term,estimate.gfcf=estimate,std.error.gfcf=std.error,df_gfcf=df.error)

    gva_base<-gva %>%
      map_df(function(x){
        get_estimates(x,vcov=vcov_cluster(x, cluster = cluster, ssc = NULL))%>%filter(grepl("frd",term))
      })%>%select(term,estimate.gva=estimate,std.error.gva=std.error,df_gva=df.error)

  }else{
    gfcf_base<-gfcf %>%map_df(function(x){
      coeftest(x,vcov=vcovHC(x))%>%tidy()%>%filter(grepl("frd",term))%>%mutate(
        df=ncol(x$W[[1L]])-length(x$coefficients)#number of degrees of freedom equal to the difference between the number of moment conditions and the number of coefficients.
      )
    })%>%select(term,estimate.gfcf=estimate,std.error.gfcf=std.error,df_gfcf=df)

    gva_base<-gva %>%
      map_df(function(x){
        coeftest(x,vcov=vcovHC(x))%>%tidy()%>%filter(grepl("frd",term))%>%mutate(
          df=ncol(x$W[[1L]])-length(x$coefficients)#number of degrees of freedom equal to the difference between the number of moment conditions and the number of coefficients.
        )
      })%>%select(term,estimate.gva=estimate,std.error.gva=std.error,df_gva=df)
  }


  left_join(gfcf_base,gva_base,by="term")%>%
    mutate(tvalue=(estimate.gva-estimate.gfcf)/sqrt(std.error.gfcf^2+std.error.gva^2),
           pvalue= case_when(
             estimate.gva>=estimate.gfcf~pt(-abs(tvalue), df_gfcf+df_gva)%>%round(4),
             TRUE~1-(pt(-abs(tvalue), df_gfcf+df_gva))%>%round(4)
           ) #2 * pt(-abs(tvalue), df_gfcf+df_gva) would be two-sided test
    )%>%select(term,tvalue,pvalue)
}
