get_arelanobond<-function(data_input,dep_var,interaction_variable=NULL,dep_var_log=TRUE,exp_var,lag=0:3,dynamic=TRUE,dyn_lag=3,dyn_lag_with_lag=FALSE,gmm_lag=NULL,collapse=FALSE,controls=c("dpi_legelec","dpi_auton","dpi_state","dpi_exelec","ameco_udgg","ameco_uigg0","ameco_uvgd","ameco_avgdgp","ameco_uucgi"),fixed_effects=c("nuts_id","year"),lag_controls=FALSE,id="nuts_id",time="year"){
  #load data
  fixed_effects=c(id,time)

  #load data
  if(class(data_input)[1]=="pdata.frame"){
    data<-data_input
  }else{
    data<-data_input%>%pdata.frame(index=c(id,time))
  }

if(!is.null(interaction_variable)&length(exp_var)!=length(interaction_variable)){
  print("exp_var and interaction_variable must be of same length if interaction_variable is not null")
}

  if(!is.null(interaction_variable)){
    interaction_name<-paste0(exp_var,"_",interaction_variable)
    if(length(exp_var)==1){
      data<-data%>%mutate(!!sym(interaction_name):=!!sym(exp_var)*!!sym(interaction_variable))
    }else if(length(exp_var==2)){
      data<-data%>%mutate(!!sym(interaction_name[1]):=!!sym(exp_var[1])*!!sym(interaction_variable[1]),
                          !!sym(interaction_name[2]):=!!sym(exp_var[2])*!!sym(interaction_variable[2]))
    }else if(length(exp_var)==3){
      data<-data%>%mutate(!!sym(interaction_name[1]):=!!sym(exp_var[1])*!!sym(interaction_variable[1]),
                          !!sym(interaction_name[2]):=!!sym(exp_var[2])*!!sym(interaction_variable[2]),
                          !!sym(interaction_name[3]):=!!sym(exp_var[3])*!!sym(interaction_variable[3]))
    }

  }

  #define dependent variable
  if(dep_var_log){
    dep_var<-paste0("log(",dep_var,")")
  }

#gmm_instruments
  if(is.null(interaction_variable)){
    if(is.null(gmm_lag)){
      if(length(exp_var)==1){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var,",",x+2,":",99,")+plm::lag(",dep_var,",",dyn_lag+2,":",99,")")
        })
      }else if(length(exp_var)==2){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",99,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",99,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",99,")")
        })
      }else if(length(exp_var)==3){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",99,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",99,")+",
                 "plm::lag(",exp_var[3],",",x+2,":",99,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",99,")")
        })
      }

    }else{
      if(length(exp_var)==1){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var,",",x+2,":",x+1+gmm_lag,")+plm::lag(",dep_var,",",dyn_lag+2,":",dyn_lag+1+gmm_lag,")")
        })
      }else if(length(exp_var)==2){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",dyn_lag+1+gmm_lag,")")
        })
      }else if(length(exp_var)==3){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",exp_var[3],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",dyn_lag+1+gmm_lag,")")
        })
      }

    }
  }else{
    if(is.null(gmm_lag)){
      if(length(exp_var)==1){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var,",",x+2,":",99,")+",
                 "plm::lag(",interaction_name,",",x+2,":",99,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",99,")")
        })
      }else if(length(exp_var)==2){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",99,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",99,")+",
                 "plm::lag(",interaction_name[1],",",x+2,":",99,")+",
                 "plm::lag(",interaction_name[2],",",x+2,":",99,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",99,")")
        })
      }else if(length(exp_var)==3){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",99,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",99,")+",
                 "plm::lag(",exp_var[3],",",x+2,":",99,")+",
                 "plm::lag(",interaction_name[1],",",x+2,":",99,")+",
                 "plm::lag(",interaction_name[2],",",x+2,":",99,")+",
                 "plm::lag(",interaction_name[3],",",x+2,":",99,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",99,")")
        })
      }

    }else{
      if(length(exp_var)==1){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var,",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",interaction_name,",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",dyn_lag+1+gmm_lag,")")
        })
      }else if(length(exp_var)==2){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",interaction_name[1],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",interaction_name[2],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",dyn_lag+1+gmm_lag,")")
        })
      }else if(length(exp_var)==3){
        gmm_instruments<-sapply(lag,function(x){
          paste0("plm::lag(",exp_var[1],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",exp_var[2],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",exp_var[3],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",interaction_name[1],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",interaction_name[2],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",interaction_name[3],",",x+2,":",x+1+gmm_lag,")+",
                 "plm::lag(",dep_var,",",dyn_lag+2,":",dyn_lag+1+gmm_lag,")")
        })
      }

    }
  }





  #define explanatory variable
  #with all lags added via 'lag' (if only one lag is desired, add only one)

  if(is.null(interaction_variable)){
    if(length(exp_var)==1){
      exp_var<-lapply(lag,function(x){
        paste0("plm::lag(",exp_var,", ",x,")")
      })
    }else if(length(exp_var)==2){
      exp_var<-lapply(lag,function(x){
        paste0("plm::lag(",exp_var[1],", ",x,")+",
               "plm::lag(",exp_var[2],", ",x,")")
      })
    }else if(length(exp_var)==3){
      exp_var<-lapply(lag,function(x){
        paste0("plm::lag(",exp_var[1],", ",x,")+",
               "plm::lag(",exp_var[2],", ",x,")+",
               "plm::lag(",exp_var[3],", ",x,")")
      })
    }

  }else{
    if(length(exp_var)==1){
      exp_var<-lapply(lag,function(x){
        paste0("plm::lag(",exp_var,", ",x,")+",
               "plm::lag(",interaction_name,", ",x,")")
      })
    }else if(length(exp_var)==2){
      exp_var<-lapply(lag,function(x){
        paste0("plm::lag(",exp_var[1],", ",x,")+",
               "plm::lag(",exp_var[2],", ",x,")+",
               "plm::lag(",interaction_name[1],", ",x,")+",
               "plm::lag(",interaction_name[2],", ",x,")+")
      })
    }else if(length(exp_var)==3){
      exp_var<-lapply(lag,function(x){
        paste0("plm::lag(",exp_var[1],", ",x,")+",
               "plm::lag(",exp_var[2],", ",x,")+",
               "plm::lag(",exp_var[3],", ",x,")+",
               "plm::lag(",interaction_name[1],", ",x,")+",
               "plm::lag(",interaction_name[2],", ",x,")+",
               "plm::lag(",interaction_name[3],", ",x,")+")
      })
    }

  }



  #define controls
  #possibility of also lagging the controls
  if(lag_controls){
    controls<-lapply(lag,function(x){
      paste0("plm::lag(",controls,", ",x,")")%>%paste(collapse = "+")
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
        paste0("plm::lag(",dep_var,",",(x+1):(dyn_lag+x),")")%>%paste(collapse = "+")
      })
    }else{
      dynamic<-lapply(lag,function(x){
        paste0("plm::lag(",dep_var,",",1:(dyn_lag+x),")")%>%paste(collapse = "+")
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
