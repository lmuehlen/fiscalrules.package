get_table<-function(l,filename="test",caption=NULL,coef.map=NULL,fontsize=NULL,digits=4,label=NULL,side=FALSE,lag_order,header,path){

  iv<-FALSE
  ab<-FALSE
  within<-FALSE
  for(i in 1:length(l)){
    if(class(l[[i]])[1]=="fixest"){
      if(!is.null(l[[i]][["iv"]])){
        iv<-TRUE
      }else{
        within<-TRUE
      }
    }else if(class(l[[i]])[1]=="pgmm"){
      ab<-TRUE
    }
  }



  gof<-list(
    "nuts_id fixed effects"=rep("$\\checkmark$",length(l)),
    "year fixe effects"=rep("$\\checkmark$",length(l))
  )




  if(within){
    within_gof<-list(
      # "Unit root test t-value"=sapply(l,function(x){if(class(x)[1]=="fixest"&is.null(x[1][["iv"]])){
      #  y<-x[["y_demeaned"]]%>%matrix(ncol=167)
      # purtest(y,pmax = 5,exo = "trend",test = "levinlin")[["statistic"]][["statistic"]]
      #}else{NA}
      # }),
      #"Unit root test p-value"=sapply(l,function(x){if(class(x)[1]=="fixest"&is.null(x[1][["iv"]])){
      #y<-x[["y_demeaned"]]%>%matrix(ncol=167)
      #  purtest(y,pmax = 5,exo = "trend",test = "levinlin")[["statistic"]][["p.value"]]
      #}else{NA}
      #})
      "Within adj. $R^2$"=sapply(l,function(x){
        if(class(x)[1]=="fixest"&is.null(x[["iv"]])){
          fitstat(x,"war2")[[1]]
        }else{NA}
      })
    )
    gof<-do.call(c,list(gof,within_gof))
  }


  if(ab){
    ab_gof<-list(
      "AR(2)"=sapply(l,function(x){if(class(x)[1]=="pgmm"){mtest(x,order=2,vcov = vcovHC(x,cluster=c("countrycode")))[["p.value"]]}else{NA}}),
      "Sargan (GMM)"=sapply(l,function(x){if(class(x)[1]=="pgmm"){sargan(x,"twostep")[["p.value"]]}else{NA}})
    )

    gof<-do.call(c,list(gof,ab_gof))
  }



  if(iv){
    iv_gof<-list(
      #F-Test
      "F-test $1^{st}$ stage"=sapply(l,function(x){
        if(class(x)[1]=="fixest"&!is.null(x[["iv"]])){
          fitstat(x,"ivf1",vcov=~countrycode)[[1]]$stat
        }else{
          NA
        }
      }),
      #Kleinbergen-Paap
      "Kleinbergen-Paap"=sapply(l,function(x){
        if(class(x)[1]=="fixest"&!is.null(x[["iv"]])){
          fitstat(x,"ivwald1",vcov=~countrycode)[[1]]$stat
        }else{
          NA
        }
      })
      ,
      #Sargan
      # "Sargan (IV)"=sapply(l,function(x) {
      #  if(class(x)[1]=="fixest"&!is.null(x[["iv"]])){
      #   f<-fitstat(x,"sargan")[[1]]
      #
      #  if(is.na(f)){
      #  NA
      #  }else{
      #  f$p
      #}
      #  }else{
      #   NA
      #  }
      #}),
      #Wu-Hausman
      "Wu-Hausman"=sapply(l,function(x) {
        if(class(x)[1]=="fixest"&!is.null(x[["iv"]])){
          f<-fitstat(x,"wh")[[1]]$p

        }else{
          NA
        }
      })
    )

    gof<-do.call(c,list(gof,iv_gof))
  }


  for(i in 1:length(l)){
    if(class(l[[i]][1])=="pgmm"){
      l[[i]]<-coeftest(l[[i]],vcov=vcovHC(l[[i]],cluster=c("countrycode")))
    }
  }

#defines coefficient map
if(is.null(coef.map)){
  coef.map<-list(
    ###########for AB
    "stats::lag(frd_lg_full, 0)"="$FR^{lg}_{t-h}$",
    "stats::lag(frd_lg_full, 1)"="$FR^{lg}_{t-h}$",
    "stats::lag(frd_lg_full, 2)"="$FR^{lg}_{t-h}$",
    "stats::lag(frd_lg_full, 3)"="$FR^{lg}_{t-h}$",
    "stats::lag(frd_lg_full, 4)"="$FR^{lg}_{t-h}$",
    "stats::lag(frd_rg_full, 0)"="$FR^{rg}_{t-h}$",
    "stats::lag(frd_rg_full, 1)"="$FR^{rg}_{t-h}$",
    "stats::lag(frd_rg_full, 2)"="$FR^{rg}_{t-h}$",
    "stats::lag(frd_rg_full, 3)"="$FR^{rg}_{t-h}$",
    "stats::lag(frd_rg_full, 4)"="$FR^{rg}_{t-h}$",
    "stats::lag(frd_sng_full, 0)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_sng_full, 1)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_sng_full, 2)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_sng_full, 3)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_sng_full, 4)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_snggg_full, 0)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_snggg_full, 1)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_snggg_full, 2)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_snggg_full, 3)"="$FR^{sng}_{t-h}$",
    "stats::lag(frd_snggg_full, 4)"="$FR^{sng}_{t-h}$",
    #gfcf_nms_pps
    "stats::lag(log(ardeco_gfcf_nms_pps), 1)"="$\\mbox{dependent variable}_{t-1}$",
    "stats::lag(log(ardeco_gfcf_nms_pps), 2)"="$\\mbox{dependent variable}_{t-2}$",
    "stats::lag(log(ardeco_gfcf_nms_pps), 3)"="$\\mbox{dependent variable}_{t-3}$",
    "stats::lag(log(ardeco_gfcf_nms_pps), 4)"="$\\mbox{dependent variable}_{t-4}$",
    "stats::lag(log(ardeco_gfcf_nms_pps), 5)"="$\\mbox{dependent variable}_{t-5}$",
    "stats::lag(log(ardeco_gfcf_nms_pps), 6)"="$\\mbox{dependent variable}_{t-6}$",
    "stats::lag(log(ardeco_gfcf_nms_pps), 7)"="$\\mbox{dependent variable}_{t-7}$",
    "stats::lag(log(ardeco_gfcf_nms_pps), 8)"="$\\mbox{dependent variable}_{t-8}$",

    #gva_nms_pps
    "stats::lag(log(ardeco_gva_nms_pps), 1)"="$\\mbox{dependent variable}_{t-1}$",
    "stats::lag(log(ardeco_gva_nms_pps), 2)"="$\\mbox{dependent variable}_{t-2}$",
    "stats::lag(log(ardeco_gva_nms_pps), 3)"="$\\mbox{dependent variable}_{t-3}$",
    "stats::lag(log(ardeco_gva_nms_pps), 4)"="$\\mbox{dependent variable}_{t-4}$",
    "stats::lag(log(ardeco_gva_nms_pps), 5)"="$\\mbox{dependent variable}_{t-5}$",
    "stats::lag(log(ardeco_gva_nms_pps), 6)"="$\\mbox{dependent variable}_{t-6}$",
    "stats::lag(log(ardeco_gva_nms_pps), 7)"="$\\mbox{dependent variable}_{t-7}$",
    "stats::lag(log(ardeco_gva_nms_pps), 8)"="$\\mbox{dependent variable}_{t-8}$",

    #gfcf_gva_ratio
    "stats::lag(ardeco_gfcf_gva_ratio, 1)"="$\\mbox{dependent variable}_{t-1}$",
    "stats::lag(ardeco_gfcf_gva_ratio, 2)"="$\\mbox{dependent variable}_{t-2}$",
    "stats::lag(ardeco_gfcf_gva_ratio, 3)"="$\\mbox{dependent variable}_{t-3}$",
    "stats::lag(ardeco_gfcf_gva_ratio, 4)"="$\\mbox{dependent variable}_{t-4}$",
    "stats::lag(ardeco_gfcf_gva_ratio, 5)"="$\\mbox{dependent variable}_{t-5}$",
    "stats::lag(ardeco_gfcf_gva_ratio, 6)"="$\\mbox{dependent variable}_{t-6}$",
    "stats::lag(ardeco_gfcf_gva_ratio, 7)"="$\\mbox{dependent variable}_{t-7}$",
    "stats::lag(ardeco_gfcf_gva_ratio, 8)"="$\\mbox{dependent variable}_{t-8}$",

    #for Within and IV
    "fit_frd_lg_full"="$FR^{lg}_{t-h}$",
    "fit_l(frd_lg_full, 1)"="$FR^{lg}_{t-h}$",
    "fit_l(frd_lg_full, 2)"="$FR^{lg}_{t-h}$",
    "fit_l(frd_lg_full, 3)"="$FR^{lg}_{t-h}$",
    "fit_l(frd_lg_full, 4)"="$FR^{lg}_{t-h}$",
    "fit_frd_rg_full"="$FR^{rg}_{t-h}$",
    "fit_l(frd_rg_full, 1)"="$FR^{rg}_{t-h}$",
    "fit_l(frd_rg_full, 2)"="$FR^{rg}_{t-h}$",
    "fit_l(frd_rg_full, 3)"="$FR^{rg}_{t-h}$",
    "fit_l(frd_rg_full, 4)"="$FR^{rg}_{t-h}$",
    "fit_frd_sng_full"="$FR^{sng}_{t-h}$",
    "fit_l(frd_sng_full, 1)"="$FR^{sng}_{t-h}$",
    "fit_l(frd_sng_full, 2)"="$FR^{sng}_{t-h}$",
    "fit_l(frd_sng_full, 3)"="$FR^{sng}_{t-h}$",
    "fit_l(frd_sng_full, 4)"="$FR^{sng}_{t-h}$",
    "fit_l(frd_sng_full, 5)"="$FR^{sng}_{t-h}$",
    "frd_lg_full"="$FR^{lg}_{t-h}$",
    "l(frd_lg_full, 1)"="$FR^{lg}_{t-h}$",
    "l(frd_lg_full, 2)"="$FR^{lg}_{t-h}$",
    "l(frd_lg_full, 3)"="$FR^{lg}_{t-h}$",
    "l(frd_lg_full, 4)"="$FR^{lg}_{t-h}$",
    "frd_rg_full"="$FR^{rg}_{t-h}$",
    "l(frd_rg_full, 1)"="$FR^{rg}_{t-h}$",
    "l(frd_rg_full, 2)"="$FR^{rg}_{t-h}$",
    "l(frd_rg_full, 3)"="$FR^{rg}_{t-h}$",
    "l(frd_rg_full, 4)"="$FR^{rg}_{t-h}$",
    "frd_sng_full"="$FR^{sng}_{t-h}$",
    "l(frd_sng_full, 1)"="$FR^{sng}_{t-h}$",
    "l(frd_sng_full, 2)"="$FR^{sng}_{t-h}$",
    "l(frd_sng_full, 3)"="$FR^{sng}_{t-h}$",
    "l(frd_sng_full, 4)"="$FR^{sng}_{t-h}$",
    "l(frd_sng_full, 5)"="$FR^{sng}_{t-h}$",
    "frd_snggg_full"="$FR^{sng}_{t-h}$",
    "l(frd_snggg_full, 1)"="$FR^{sng}_{t-h}$",
    "l(frd_snggg_full, 2)"="$FR^{sng}_{t-h}$",
    "l(frd_snggg_full, 3)"="$FR^{sng}_{t-h}$",
    "l(frd_snggg_full, 4)"="$FR^{sng}_{t-h}$",
    "l(frd_snggg_full, 5)"="$FR^{sng}_{t-h}$",
    #gfcf_nms_pps
    "l(log(ardeco_gfcf_nms_pps), 1)"="$\\mbox{dependent variable}_{t-1}$",
    "l(log(ardeco_gfcf_nms_pps), 2)"="$\\mbox{dependent variable}_{t-2}$",
    "l(log(ardeco_gfcf_nms_pps), 3)"="$\\mbox{dependent variable}_{t-3}$",
    "l(log(ardeco_gfcf_nms_pps), 4)"="$\\mbox{dependent variable}_{t-4}$",
    "l(log(ardeco_gfcf_nms_pps), 5)"="$\\mbox{dependent variable}_{t-5}$",
    "l(log(ardeco_gfcf_nms_pps), 6)"="$\\mbox{dependent variable}_{t-6}$",
    "l(log(ardeco_gfcf_nms_pps), 7)"="$\\mbox{dependent variable}_{t-7}$",
    "l(log(ardeco_gfcf_nms_pps), 8)"="$\\mbox{dependent variable}_{t-8}$",

    #gva_nms_pps
    "l(log(ardeco_gva_nms_pps), 1)"="$\\mbox{dependent variable}_{t-1}$",
    "l(log(ardeco_gva_nms_pps), 2)"="$\\mbox{dependent variable}_{t-2}$",
    "l(log(ardeco_gva_nms_pps), 3)"="$\\mbox{dependent variable}_{t-3}$",
    "l(log(ardeco_gva_nms_pps), 4)"="$\\mbox{dependent variable}_{t-4}$",
    "l(log(ardeco_gva_nms_pps), 5)"="$\\mbox{dependent variable}_{t-5}$",
    "l(log(ardeco_gva_nms_pps), 6)"="$\\mbox{dependent variable}_{t-6}$",
    "l(log(ardeco_gva_nms_pps), 7)"="$\\mbox{dependent variable}_{t-7}$",
    "l(log(ardeco_gva_nms_pps), 8)"="$\\mbox{dependent variable}_{t-8}$",

    #gfcf_gva_ratio
    "l(ardeco_gfcf_gva_ratio, 1)"="$\\mbox{dependent variable}_{t-1}$",
    "l(ardeco_gfcf_gva_ratio, 2)"="$\\mbox{dependent variable}_{t-2}$",
    "l(ardeco_gfcf_gva_ratio, 3)"="$\\mbox{dependent variable}_{t-3}$",
    "l(ardeco_gfcf_gva_ratio, 4)"="$\\mbox{dependent variable}_{t-4}$",
    "l(ardeco_gfcf_gva_ratio, 5)"="$\\mbox{dependent variable}_{t-5}$",
    "l(ardeco_gfcf_gva_ratio, 6)"="$\\mbox{dependent variable}_{t-6}$",
    "l(ardeco_gfcf_gva_ratio, 7)"="$\\mbox{dependent variable}_{t-7}$",
    "l(ardeco_gfcf_gva_ratio, 8)"="$\\mbox{dependent variable}_{t-8}$"

  )
}

  texreg(l=l,
         file = paste0(path,filename,".txt"),
         fontsize = fontsize,
         stars = c( 0.01, 0.05,0.1),
         robust=TRUE,
         omit.coef = "dpi|ameco",
         custom.coef.map=coef.map,
         include.wald=FALSE,
         include.nobs=FALSE,
         include.sargan=FALSE,
         include.rsquared=FALSE,
         include.adjrs=FALSE,
         include.proj.stats=FALSE,
         include.groups=FALSE,
         custom.gof.rows =gof,
         booktabs = TRUE,
         label = label,
         sideways = side,
         use.packages = FALSE,
         float.pos = "h!",
         digits = digits,
         custom.model.names = paste0("h=",lag_order),
         custom.header = header,
         caption = caption
  )

}


