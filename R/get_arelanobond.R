get_arelanobond<-function(data_input,dep_varinteraction_variable=NULL,dep_var_log=TRUE,exp_var,lag=0:3,dynamic=TRUE,dyn_lag=3,dyn_lag_with_lag=FALSE,gmm_lag=NULL,collapse=FALSE,controls=c("dpi_legelec","dpi_auton","dpi_state","dpi_exelec","ameco_udgg","ameco_uigg0","ameco_uvgd","ameco_avgdgp","ameco_uucgi"),fixed_effects=c("nuts_id","year"),lag_controls=FALSE,id="nuts_id",time="year"){
  #load data
  fixed_effects=c(id,time)

  #load data
  if(class(data_input)[1]=="pdata.frame"){
    data<-data_input
  }else{
    data<-data_input%>%pdata.frame(index=c(id,time))
  }


  #define dependent variable
  if(dep_var_log){
    dep_var<-paste0("log(",dep_var,")")
  }

  if(is.null(gmm_lag)){
    gmm_instruments<-sapply(lag,function(x){
      paste0("stats::lag(",exp_var,",",x+1,":",99,")+stats::lag(",dep_var,",",3,":",99,")")
    })
  }else{
    gmm_instruments<-sapply(lag,function(x){
      paste0("stats::lag(",exp_var,",",x+1,":",x+gmm_lag,")+stats::lag(",dep_var,",",3,":",3+gmm_lag,")")
    })
  }




  #define explanatory variable
  #with all lags added via 'lag' (if only one lag is desired, add only one)
  exp_var<-lapply(lag,function(x){
    paste0("stats::lag(",exp_var,", ",x,")")
  })

  #define interaction term
  if(!is.null(interaction_variable)){
  exp_var<-lapply(lag,function(x){
    paste0("stats::lag(",exp_var,"*",interaction_variable,", ",x,")")
  })
  }

  #define controls
  #possibility of also lagging the controls
  if(lag_controls){
    controls<-lapply(lag,function(x){
      paste0("stats::lag(",controls,", ",x,")")%>%paste(collapse = "+")
    })
  }else{
    controls<-paste(controls,collapse = "+")
  }

  #add lagged dependent varibale to controls if dynamic==TRUE
  #only include lagged dependent variables starting at the time of the explanatory variable if 'dyn_lag_with_lag'==TRUE (otherwise starting always with lag of 1)
  #generally includes 'dyn_lag' lags of the dependent variable starting at the time of the explanatory variable
  if(dynamic){
    if(dyn_lag_with_lag){
      dynamic<-lapply(lag,function(x){
        paste0("stats::lag(",dep_var,",",(x+1):(dyn_lag+x),")")%>%paste(collapse = "+")
      })
    }else{
      dynamic<-lapply(lag,function(x){
        paste0("stats::lag(",dep_var,",",1:(dyn_lag+x),")")%>%paste(collapse = "+")
      })
    }
    controls<-paste0(dynamic,"+",controls)
  }

  formula<-paste0(dep_var,"~",exp_var,"+",controls,"|",gmm_instruments)

  formula<-lapply(formula,function(x){
    as.formula(x)
  })

  reg<-lapply(formula,function(x){
    pgmm(x,
         data,
         effect = "twoways",
         collapse = collapse,
         fsm="I")
  })


  return(reg)
}
