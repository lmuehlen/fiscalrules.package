get_fixedeffect_and_iv<-function(data_input,dep_var,interaction_variable=NULL,dep_var_log=TRUE,exp_var,lag=0:3,dynamic=TRUE,dyn_lag=3,dyn_lag_with_lag=FALSE, IV=TRUE,inst_var=c("dpi_govfrac"),lag_inst_var=2,controls,fixed_effects=c("countrycode"),id="nuts_id",time="year"){


fixed_effects=c(id,time)

  #load data
  if(class(data_input)[1]=="fixest_panel"){
    data<-data_input
  }else{
    data<-data_input%>%panel(panel.id=c(id,time))
  }



  #define dependent variable
  if(dep_var_log){
    dep_var<-paste0("log(",dep_var,")")
  }


  #define explanatory variable
  #with all lags added via 'lag' (if only one lag is desired, add only one)
  #resuls in x model specifications differing in the lag of the explanatory variable
  exp_var<-sapply(lag,function(x){
    paste0("l(",exp_var,", ",x,")")
  })


  #define interaction term
  if(!is.null(interaction_variable)){
  interaction<-sapply(lag,function(x){
    paste0("l(",exp_var,"*",interaction_variable,", ",x,")")
  })
  }



  #define instrument variables lagged by 'lag_inst_var'
  if(length(lag_inst_var)==1){
    inst_var<-lapply(lag,function(x){
      paste0("l(",inst_var,", ",(x+lag_inst_var),")")%>%paste(collapse = "+")
    })
  }else{
    inst_var<-lapply(lag,function(x){
      paste0("l(",inst_var,", ",(x+min(lag_inst_var)),":",(x+max(lag_inst_var)),")")%>%paste(collapse = "+")
    })
  }


  #define controls

  controls<-paste(controls,collapse = "+")


  #add lagged dependent varibale to controls if dynamic==TRUE
  #only include lagged dependent variables starting at the time of the explanatory variable if 'dyn_lag_with_lag'==TRUE (otherwise starting always with lag of 1)
  #generally includes 'dyn_lag' lags of the dependent variable starting at the time of the explanatory variable
  if(dynamic){
    if(dyn_lag_with_lag){
      dynamic<-sapply(lag,function(x){
        paste0("l(",dep_var,",",(x+1),":",(dyn_lag+x),")")

      })
    }else{
      dynamic<-sapply(lag,function(x){
        paste0("l(",dep_var,",1:",(dyn_lag+x),")")
      })
    }
    controls<-paste0(dynamic,"+",controls)
  }

  #define fixed effects
  fixed_effects<-paste(fixed_effects,collapse = "+")

  #paste components of formula
  if(IV){
    if(!is.null(interaction_variable)){
      formula<-paste0(dep_var,"~",controls,"|",fixed_effects,"|",exp_var,"+",interaction,"~",inst_var)
    }else{
      formula<-paste0(dep_var,"~",controls,"|",fixed_effects,"|",exp_var,"~",inst_var)
    }

  }else{
    if(!is.null(interaction_variable)){
      formula<-paste0(dep_var,"~",exp_var,"+",interaction,"+",controls,"|",fixed_effects)
    }else{
      formula<-paste0(dep_var,"~",exp_var,"+",controls,"|",fixed_effects)
    }
  }

  #estimation of models (the number of the models is the number of the lags of the dependent variable)
  m<-lapply(formula,function(x){
    fixest::feols(as.formula(x),data = data,
          vcov = ~countrycode)
  })

  return(m)
}
