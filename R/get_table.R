get_table<-function(l,filename="test",cluster_name="nuts_id",caption=NULL,coef.map=NULL,fontsize=NULL,scalebox=0.8,digits=4,label=NULL,side=FALSE,lag_order,headers,path,afteryears=10,include_effect_afterx=TRUE,include_t=TRUE){

if(is.null(label)){
  label<-filename
}

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



if(within==TRUE|ab==TRUE){
    se<-list()
    p<-list()
  for(i in 1:length(l)){
      if(class(l[[i]][1])=="pgmm"){
        se[[i]]<-0
        p[[i]]<-0
      }else{
        se[[i]]<-se(l[[i]],cluster=cluster_name)
        p[[i]]<-pvalue(l[[i]],cluster=cluster_name)
      }
    }
  }else{
    se<-0
    p<-0
  }



## GOF

  gof<-list(
    "F.E.(Nuts2)"=rep("$\\checkmark$",length(l)),
    "F.E.(Year)"=rep("$\\checkmark$",length(l)),
    "Macroeconomic controls"=rep("$\\checkmark$",length(l)),
    "Institutional controls"=rep("$\\checkmark$",length(l))
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
    gof<-c(gof,within_gof)
  }


  if(ab){
    ab_gof<-list(
      "AR(2)"=sapply(l,function(x){if(class(x)[1]=="pgmm"){mtest(x,order=2,vcov = vcovHC(x))[["p.value"]]}else{NA}}),
      "Sargan (GMM)"=sapply(l,function(x){if(class(x)[1]=="pgmm"){sargan(x,"twostep")[["p.value"]]}else{NA}})
    )

    gof<-c(gof,ab_gof)
  }



  if(iv){
    iv_gof<-list(
      #F-Test
      "F-test $1^{st}$ stage"=sapply(l,function(x){
        if(class(x)[1]=="fixest"&!is.null(x[["iv"]])){
          fitstat(x,"ivf1",cluster=cluster_name)[[1]]$stat
        }else{
          NA
        }
      }),
      #Kleinbergen-Paap
      "Kleinbergen-Paap"=sapply(l,function(x){
        if(class(x)[1]=="fixest"&!is.null(x[["iv"]])){
          fitstat(x,"ivwald1",cluster=cluster_name)[[1]]$stat
        }else{
          NA
        }
      })
      ,
      #Wu-Hausman
      "Wu-Hausman"=sapply(l,function(x) {
        if(class(x)[1]=="fixest"&!is.null(x[["iv"]])){
          f<-fitstat(x,"wh")[[1]]$p

        }else{
          NA
        }
      })
    )

    gof<-c(gof,iv_gof)
  }

if(include_effect_afterx){
#additional gof for all
  #effect after x years
effect_afterx<-sapply(l,function(x){
    dyn_effect<-coef(x)[2:4]
    shortrun<-coef(x)[1]
    vcov<-vcov(x,cluster=cluster_name)[c(2:4,1),c(2:4,1)]
    get_effect_after_x_years(shortrun,dyn_effect,afteryears,vcov,report="effect")
  })


se_afterx<-sapply(l,function(x){
    dyn_effect<-coef(x)[2:4]
    shortrun<-coef(x)[1]
    vcov<-vcov(x,cluster=cluster_name)[c(2:4,1),c(2:4,1)]
    get_effect_after_x_years(shortrun,dyn_effect,afteryears,vcov,report="se")
  })

tvalue<-effect_afterx/se_afterx

if(class(l[[1]])[1]=="fixest"){
df<-sapply(l,function(x){degrees_freedom(x,"t")})
}else{
df<-sapply(l,function(x){df=ncol(x$W[[1L]])-length(x$coefficients)})
}
pvalues<-2 * pt(-abs(tvalue), df)
stars<-case_when(pvalues<0.01~"***",
                 pvalues<0.05~"**",
                 pvalues<0.1~"*",
                 TRUE~"")

effect_afterx<-paste0("$",sprintf("%.4f",round(effect_afterx,4)),"^{",stars,"}$")
  se_afterx<-paste0("$(",sprintf("%.4f",round(se_afterx,4)),")$")


name_effect<-paste("Effect after",afteryears,"years")

gof_afterx<-list(
  effect_afterx,
  se_afterx
)%>%setNames(c(name_effect,""))


gof<-c(gof,gof_afterx)
}

if(include_t){
results_ttest<-rep("-",length(l))
results_ttest[(length(l)/2+1):length(l)]<-t_test_comp_gfcfgva(l[1:(length(l)/2)],l[(length(l)/2+1):length(l)])[3][[1]]
gof_ttest<-list("P-value: Investment>Expenditure"=paste0("$",results_ttest,"$"))
gof<-c(gof,gof_ttest)
}

# get coefficients and se of pgmm
  for(i in 1:length(l)){
    if(class(l[[i]][1])=="pgmm"){
      l[[i]]<-coeftest(l[[i]],vcov=vcovHC(l[[i]]))#Windmejer adjusted SE
    }
  }




#defines coefficient map
if(is.null(coef.map)){
  coef.map<-list(
    ###########for AB
    #full
    "plm::lag(frd_lg_full, 0)"="$FR^{lg}_{t-h}$",
    "plm::lag(frd_lg_full, 1)"="$FR^{lg}_{t-h}$",
    "plm::lag(frd_lg_full, 2)"="$FR^{lg}_{t-h}$",
    "plm::lag(frd_lg_full, 3)"="$FR^{lg}_{t-h}$",
    "plm::lag(frd_lg_full, 4)"="$FR^{lg}_{t-h}$",
    "plm::lag(frd_rg_full, 0)"="$FR^{rg}_{t-h}$",
    "plm::lag(frd_rg_full, 1)"="$FR^{rg}_{t-h}$",
    "plm::lag(frd_rg_full, 2)"="$FR^{rg}_{t-h}$",
    "plm::lag(frd_rg_full, 3)"="$FR^{rg}_{t-h}$",
    "plm::lag(frd_rg_full, 4)"="$FR^{rg}_{t-h}$",
    "plm::lag(frd_sng_full, 0)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_sng_full, 1)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_sng_full, 2)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_sng_full, 3)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_sng_full, 4)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_snggg_full, 0)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_snggg_full, 1)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_snggg_full, 2)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_snggg_full, 3)"="$FR^{sng}_{t-h}$",
    "plm::lag(frd_snggg_full, 4)"="$FR^{sng}_{t-h}$",
    #full-interactions
    "plm::lag(frd_lg_full_after_gfc, 0)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_full_after_gfc, 1)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_full_after_gfc, 2)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_full_after_gfc, 3)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_full_after_gfc, 4)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_full_after_gfc, 0)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_full_after_gfc, 1)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_full_after_gfc, 2)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_full_after_gfc, 3)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_full_after_gfc, 4)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_full_after_gfc, 0)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_full_after_gfc, 1)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_full_after_gfc, 2)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_full_after_gfc, 3)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_full_after_gfc, 4)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_full_after_gfc, 0)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_full_after_gfc, 1)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_full_after_gfc, 2)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_full_after_gfc, 3)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_full_after_gfc, 4)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    #bbr
    "plm::lag(frd_lg_bbr, 0)"="$FR^{lg\\_bbr}_{t-h}$",
    "plm::lag(frd_lg_bbr, 1)"="$FR^{lg\\_bbr}_{t-h}$",
    "plm::lag(frd_lg_bbr, 2)"="$FR^{lg\\_bbr}_{t-h}$",
    "plm::lag(frd_lg_bbr, 3)"="$FR^{lg\\_bbr}_{t-h}$",
    "plm::lag(frd_lg_bbr, 4)"="$FR^{lg\\_bbr}_{t-h}$",
    "plm::lag(frd_rg_bbr, 0)"="$FR^{rg\\_bbr}_{t-h}$",
    "plm::lag(frd_rg_bbr, 1)"="$FR^{rg\\_bbr}_{t-h}$",
    "plm::lag(frd_rg_bbr, 2)"="$FR^{rg\\_bbr}_{t-h}$",
    "plm::lag(frd_rg_bbr, 3)"="$FR^{rg\\_bbr}_{t-h}$",
    "plm::lag(frd_rg_bbr, 4)"="$FR^{rg\\_bbr}_{t-h}$",
    "plm::lag(frd_sng_bbr, 0)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_sng_bbr, 1)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_sng_bbr, 2)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_sng_bbr, 3)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_sng_bbr, 4)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_snggg_bbr, 0)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_snggg_bbr, 1)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_snggg_bbr, 2)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_snggg_bbr, 3)"="$FR^{sng\\_bbr}_{t-h}$",
    "plm::lag(frd_snggg_bbr, 4)"="$FR^{sng\\_bbr}_{t-h}$",
    #bbr-interactions
    "plm::lag(frd_lg_bbr_after_gfc, 0)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_bbr_after_gfc, 1)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_bbr_after_gfc, 2)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_bbr_after_gfc, 3)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_bbr_after_gfc, 4)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_bbr_after_gfc, 0)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_bbr_after_gfc, 1)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_bbr_after_gfc, 2)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_bbr_after_gfc, 3)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_bbr_after_gfc, 4)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_bbr_after_gfc, 0)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_bbr_after_gfc, 1)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_bbr_after_gfc, 2)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_bbr_after_gfc, 3)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_bbr_after_gfc, 4)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_bbr_after_gfc, 0)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_bbr_after_gfc, 1)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_bbr_after_gfc, 2)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_bbr_after_gfc, 3)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_bbr_after_gfc, 4)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    #bbr-interactions
    "plm::lag(frd_lg_bbr_frd_lg_exclusions_bbr, 0)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_bbr_frd_lg_exclusions_bbr, 1)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_bbr_frd_lg_exclusions_bbr, 2)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_bbr_frd_lg_exclusions_bbr, 3)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_bbr_frd_lg_exclusions_bbr, 4)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_bbr_frd_rg_exclusions_bbr, 0)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_bbr_frd_rg_exclusions_bbr, 1)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_bbr_frd_rg_exclusions_bbr, 2)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_bbr_frd_rg_exclusions_bbr, 3)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_bbr_frd_rg_exclusions_bbr, 4)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_bbr_frd_sng_exclusions_bbr, 0)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_bbr_frd_sng_exclusions_bbr, 1)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_bbr_frd_sng_exclusions_bbr, 2)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_bbr_frd_sng_exclusions_bbr, 3)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_bbr_frd_sng_exclusions_bbr, 4)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_bbr_frd_snggg_exclusions_bbr, 0)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_bbr_frd_snggg_exclusions_bbr, 1)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_bbr_frd_snggg_exclusions_bbr, 2)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_bbr_frd_snggg_exclusions_bbr, 3)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_bbr_frd_snggg_exclusions_bbr, 4)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    #dr
    "plm::lag(frd_lg_dr, 0)"="$FR^{lg\\_dr}_{t-h}$",
    "plm::lag(frd_lg_dr, 1)"="$FR^{lg\\_dr}_{t-h}$",
    "plm::lag(frd_lg_dr, 2)"="$FR^{lg\\_dr}_{t-h}$",
    "plm::lag(frd_lg_dr, 3)"="$FR^{lg\\_dr}_{t-h}$",
    "plm::lag(frd_lg_dr, 4)"="$FR^{lg\\_dr}_{t-h}$",
    "plm::lag(frd_rg_dr, 0)"="$FR^{rg\\_dr}_{t-h}$",
    "plm::lag(frd_rg_dr, 1)"="$FR^{rg\\_dr}_{t-h}$",
    "plm::lag(frd_rg_dr, 2)"="$FR^{rg\\_dr}_{t-h}$",
    "plm::lag(frd_rg_dr, 3)"="$FR^{rg\\_dr}_{t-h}$",
    "plm::lag(frd_rg_dr, 4)"="$FR^{rg\\_dr}_{t-h}$",
    "plm::lag(frd_sng_dr, 0)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_sng_dr, 1)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_sng_dr, 2)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_sng_dr, 3)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_sng_dr, 4)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_snggg_dr, 0)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_snggg_dr, 1)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_snggg_dr, 2)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_snggg_dr, 3)"="$FR^{sng\\_dr}_{t-h}$",
    "plm::lag(frd_snggg_dr, 4)"="$FR^{sng\\_dr}_{t-h}$",
    #dr-interactions
    "plm::lag(frd_lg_dr_after_gfc, 0)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_dr_after_gfc, 1)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_dr_after_gfc, 2)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_dr_after_gfc, 3)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_dr_after_gfc, 4)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_dr_after_gfc, 0)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_dr_after_gfc, 1)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_dr_after_gfc, 2)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_dr_after_gfc, 3)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_dr_after_gfc, 4)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_dr_after_gfc, 0)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_dr_after_gfc, 1)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_dr_after_gfc, 2)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_dr_after_gfc, 3)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_dr_after_gfc, 4)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_dr_after_gfc, 0)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_dr_after_gfc, 1)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_dr_after_gfc, 2)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_dr_after_gfc, 3)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_dr_after_gfc, 4)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    #dr-interactions
    "plm::lag(frd_lg_dr_frd_lg_exclusions_dr, 0)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_dr_frd_lg_exclusions_dr, 1)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_dr_frd_lg_exclusions_dr, 2)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_dr_frd_lg_exclusions_dr, 3)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_lg_dr_frd_lg_exclusions_dr, 4)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_dr_frd_rg_exclusions_dr, 0)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_dr_frd_rg_exclusions_dr, 1)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_dr_frd_rg_exclusions_dr, 2)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_dr_frd_rg_exclusions_dr, 3)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_rg_dr_frd_rg_exclusions_dr, 4)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_dr_frd_sng_exclusions_dr, 0)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_dr_frd_sng_exclusions_dr, 1)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_dr_frd_sng_exclusions_dr, 2)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_dr_frd_sng_exclusions_dr, 3)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_sng_dr_frd_sng_exclusions_dr, 4)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_dr_frd_snggg_exclusions_dr, 0)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_dr_frd_snggg_exclusions_dr, 1)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_dr_frd_snggg_exclusions_dr, 2)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_dr_frd_snggg_exclusions_dr, 3)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_dr_frd_snggg_exclusions_dr, 4)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    #er
    "plm::lag(frd_lg_er, 0)"="$FR^{lg\\_er}_{t-h}$",
    "plm::lag(frd_lg_er, 1)"="$FR^{lg\\_er}_{t-h}$",
    "plm::lag(frd_lg_er, 2)"="$FR^{lg\\_er}_{t-h}$",
    "plm::lag(frd_lg_er, 3)"="$FR^{lg\\_er}_{t-h}$",
    "plm::lag(frd_lg_er, 4)"="$FR^{lg\\_er}_{t-h}$",
    "plm::lag(frd_rg_er, 0)"="$FR^{rg\\_er}_{t-h}$",
    "plm::lag(frd_rg_er, 1)"="$FR^{rg\\_er}_{t-h}$",
    "plm::lag(frd_rg_er, 2)"="$FR^{rg\\_er}_{t-h}$",
    "plm::lag(frd_rg_er, 3)"="$FR^{rg\\_er}_{t-h}$",
    "plm::lag(frd_rg_er, 4)"="$FR^{rg\\_er}_{t-h}$",
    "plm::lag(frd_sng_er, 0)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_sng_er, 1)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_sng_er, 2)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_sng_er, 3)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_sng_er, 4)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_snggg_er, 0)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_snggg_er, 1)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_snggg_er, 2)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_snggg_er, 3)"="$FR^{sng\\_er}_{t-h}$",
    "plm::lag(frd_snggg_er, 4)"="$FR^{sng\\_er}_{t-h}$",
    #er-interactions
    "plm::lag(frd_lg_er_after_gfc, 0)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_er_after_gfc, 1)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_er_after_gfc, 2)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_er_after_gfc, 3)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_lg_er_after_gfc, 4)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_er_after_gfc, 0)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_er_after_gfc, 1)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_er_after_gfc, 2)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_er_after_gfc, 3)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_rg_er_after_gfc, 4)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_er_after_gfc, 0)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_er_after_gfc, 1)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_er_after_gfc, 2)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_er_after_gfc, 3)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_sng_er_after_gfc, 4)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_er_after_gfc, 0)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_er_after_gfc, 1)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_er_after_gfc, 2)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_er_after_gfc, 3)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "plm::lag(frd_snggg_er_after_gfc, 4)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    #er-interactions
    "plm::lag(frd_lg_er_frd_lg_exclusions_er, 0)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_lg_er_frd_lg_exclusions_er, 1)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_lg_er_frd_lg_exclusions_er, 2)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_lg_er_frd_lg_exclusions_er, 3)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_lg_er_frd_lg_exclusions_er, 4)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_rg_er_frd_rg_exclusions_er, 0)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_rg_er_frd_rg_exclusions_er, 1)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_rg_er_frd_rg_exclusions_er, 2)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_rg_er_frd_rg_exclusions_er, 3)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_rg_er_frd_rg_exclusions_er, 4)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_sng_er_frd_sng_exclusions_er, 0)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_sng_er_frd_sng_exclusions_er, 1)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_sng_er_frd_sng_exclusions_er, 2)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_sng_er_frd_sng_exclusions_er, 3)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_sng_er_frd_sng_exclusions_er, 4)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_er_frd_snggg_exclusions_er, 0)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_er_frd_snggg_exclusions_er, 1)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_er_frd_snggg_exclusions_er, 2)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_er_frd_snggg_exclusions_er, 3)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "plm::lag(frd_snggg_er_frd_snggg_exclusions_er, 4)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    #gfcf_nms_pps
    "plm::lag(log(ardeco_gfcf_nms_pps), 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(log(ardeco_gfcf_nms_pps), 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(log(ardeco_gfcf_nms_pps), 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(log(ardeco_gfcf_nms_pps), 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(log(ardeco_gfcf_nms_pps), 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(log(ardeco_gfcf_nms_pps), 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(log(ardeco_gfcf_nms_pps), 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(log(ardeco_gfcf_nms_pps), 8)"="$\\mbox{dep var}_{t-8}$",

    #gva_nms_pps
    "plm::lag(log(ardeco_gva_nms_pps), 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(log(ardeco_gva_nms_pps), 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(log(ardeco_gva_nms_pps), 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(log(ardeco_gva_nms_pps), 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(log(ardeco_gva_nms_pps), 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(log(ardeco_gva_nms_pps), 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(log(ardeco_gva_nms_pps), 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(log(ardeco_gva_nms_pps), 8)"="$\\mbox{dep var}_{t-8}$",

    #gfcf_nms_cp
    "plm::lag(log(ardeco_gfcf_nms_cp), 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(log(ardeco_gfcf_nms_cp), 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(log(ardeco_gfcf_nms_cp), 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(log(ardeco_gfcf_nms_cp), 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(log(ardeco_gfcf_nms_cp), 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(log(ardeco_gfcf_nms_cp), 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(log(ardeco_gfcf_nms_cp), 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(log(ardeco_gfcf_nms_cp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gva_nms_cp
    "plm::lag(log(ardeco_gva_nms_cp), 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(log(ardeco_gva_nms_cp), 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(log(ardeco_gva_nms_cp), 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(log(ardeco_gva_nms_cp), 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(log(ardeco_gva_nms_cp), 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(log(ardeco_gva_nms_cp), 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(log(ardeco_gva_nms_cp), 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(log(ardeco_gva_nms_cp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gva_nms_gdp
    "plm::lag(log(ardeco_gva_nms_gdp), 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(log(ardeco_gva_nms_gdp), 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(log(ardeco_gva_nms_gdp), 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(log(ardeco_gva_nms_gdp), 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(log(ardeco_gva_nms_gdp), 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(log(ardeco_gva_nms_gdp), 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(log(ardeco_gva_nms_gdp), 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(log(ardeco_gva_nms_gdp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gfcf_nms_gdp
    "plm::lag(log(ardeco_gfcf_gdp), 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(log(ardeco_gfcf_gdp), 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(log(ardeco_gfcf_gdp), 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(log(ardeco_gfcf_gdp), 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(log(ardeco_gfcf_gdp), 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(log(ardeco_gfcf_gdp), 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(log(ardeco_gfcf_gdp), 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(log(ardeco_gfcf_gdp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gva_nms_gdp
    "plm::lag(log(ardeco_gva_gdp), 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(log(ardeco_gva_gdp), 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(log(ardeco_gva_gdp), 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(log(ardeco_gva_gdp), 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(log(ardeco_gva_gdp), 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(log(ardeco_gva_gdp), 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(log(ardeco_gva_gdp), 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(log(ardeco_gva_gdp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gfcf_gva_ratio
    "plm::lag(ardeco_gfcf_gva_ratio, 1)"="$\\mbox{dep var}_{t-1}$",
    "plm::lag(ardeco_gfcf_gva_ratio, 2)"="$\\mbox{dep var}_{t-2}$",
    "plm::lag(ardeco_gfcf_gva_ratio, 3)"="$\\mbox{dep var}_{t-3}$",
    "plm::lag(ardeco_gfcf_gva_ratio, 4)"="$\\mbox{dep var}_{t-4}$",
    "plm::lag(ardeco_gfcf_gva_ratio, 5)"="$\\mbox{dep var}_{t-5}$",
    "plm::lag(ardeco_gfcf_gva_ratio, 6)"="$\\mbox{dep var}_{t-6}$",
    "plm::lag(ardeco_gfcf_gva_ratio, 7)"="$\\mbox{dep var}_{t-7}$",
    "plm::lag(ardeco_gfcf_gva_ratio, 8)"="$\\mbox{dep var}_{t-8}$",

    #for Within and IV
    #full
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
    #full-interaction gfc
    "frd_lg_full:after_gfc"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_full, 1):l(after_gfc, 1)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_full, 2):l(after_gfc, 2)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_full, 3):l(after_gfc, 3)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_full, 4):l(after_gfc, 4)"="$FR^{lg\\_full}_{t-h}\\mbox{After GFC}$",
    "frd_rg_full:after_gfc"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_full, 1):l(after_gfc, 1)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_full, 2):l(after_gfc, 2)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_full, 3):l(after_gfc, 3)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_full, 4):l(after_gfc, 4)"="$FR^{rg\\_full}_{t-h}\\mbox{After GFC}$",
    "frd_sng_full:after_gfc"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_full, 1):l(after_gfc, 1)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_full, 2):l(after_gfc, 2)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_full, 3):l(after_gfc, 3)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_full, 4):l(after_gfc, 4)"="$FR^{sng\\_full}_{t-h}\\mbox{After GFC}$",
    "frd_snggg_full:after_gfc"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_full, 1):l(after_gfc, 1)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_full, 2):l(after_gfc, 2)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_full, 3):l(after_gfc, 3)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_full, 4):l(after_gfc, 4)"="$FR^{snggg\\_full}_{t-h}\\mbox{After GFC}$",
    #bbr
    "fit_frd_lg_bbr"="$FR^{lg\\_bbr}_{t-h}$",
    "fit_l(frd_lg_bbr, 1)"="$FR^{lg\\_bbr}_{t-h}$",
    "fit_l(frd_lg_bbr, 2)"="$FR^{lg\\_bbr}_{t-h}$",
    "fit_l(frd_lg_bbr, 3)"="$FR^{lg\\_bbr}_{t-h}$",
    "fit_l(frd_lg_bbr, 4)"="$FR^{lg\\_bbr}_{t-h}$",
    "fit_frd_rg_bbr"="$FR^{rg\\_bbr}_{t-h}$",
    "fit_l(frd_rg_bbr, 1)"="$FR^{rg\\_bbr}_{t-h}$",
    "fit_l(frd_rg_bbr, 2)"="$FR^{rg\\_bbr}_{t-h}$",
    "fit_l(frd_rg_bbr, 3)"="$FR^{rg\\_bbr}_{t-h}$",
    "fit_l(frd_rg_bbr, 4)"="$FR^{rg\\_bbr}_{t-h}$",
    "fit_frd_sng_bbr"="$FR^{sng\\_bbr}_{t-h}$",
    "fit_l(frd_sng_bbr, 1)"="$FR^{sng\\_bbr}_{t-h}$",
    "fit_l(frd_sng_bbr, 2)"="$FR^{sng\\_bbr}_{t-h}$",
    "fit_l(frd_sng_bbr, 3)"="$FR^{sng\\_bbr}_{t-h}$",
    "fit_l(frd_sng_bbr, 4)"="$FR^{sng\\_bbr}_{t-h}$",
    "fit_l(frd_sng_bbr, 5)"="$FR^{sng\\_bbr}_{t-h}$",
    "frd_lg_bbr"="$FR^{lg\\_bbr}_{t-h}$",
    "l(frd_lg_bbr, 1)"="$FR^{lg\\_bbr}_{t-h}$",
    "l(frd_lg_bbr, 2)"="$FR^{lg\\_bbr}_{t-h}$",
    "l(frd_lg_bbr, 3)"="$FR^{lg\\_bbr}_{t-h}$",
    "l(frd_lg_bbr, 4)"="$FR^{lg\\_bbr}_{t-h}$",
    "frd_rg_bbr"="$FR^{rg\\_bbr}_{t-h}$",
    "l(frd_rg_bbr, 1)"="$FR^{rg\\_bbr}_{t-h}$",
    "l(frd_rg_bbr, 2)"="$FR^{rg\\_bbr}_{t-h}$",
    "l(frd_rg_bbr, 3)"="$FR^{rg\\_bbr}_{t-h}$",
    "l(frd_rg_bbr, 4)"="$FR^{rg\\_bbr}_{t-h}$",
    "frd_sng_bbr"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_sng_bbr, 1)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_sng_bbr, 2)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_sng_bbr, 3)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_sng_bbr, 4)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_sng_bbr, 5)"="$FR^{sng\\_bbr}_{t-h}$",
    "frd_snggg_bbr"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_snggg_bbr, 1)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_snggg_bbr, 2)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_snggg_bbr, 3)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_snggg_bbr, 4)"="$FR^{sng\\_bbr}_{t-h}$",
    "l(frd_snggg_bbr, 5)"="$FR^{sng\\_bbr}_{t-h}$",
    #bbr-interaction gfc
    "frd_lg_bbr:after_gfc"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_bbr, 1):l(after_gfc, 1)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_bbr, 2):l(after_gfc, 2)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_bbr, 3):l(after_gfc, 3)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_bbr, 4):l(after_gfc, 4)"="$FR^{lg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "frd_rg_bbr:after_gfc"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_bbr, 1):l(after_gfc, 1)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_bbr, 2):l(after_gfc, 2)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_bbr, 3):l(after_gfc, 3)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_bbr, 4):l(after_gfc, 4)"="$FR^{rg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "frd_sng_bbr:after_gfc"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_bbr, 1):l(after_gfc, 1)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_bbr, 2):l(after_gfc, 2)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_bbr, 3):l(after_gfc, 3)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_bbr, 4):l(after_gfc, 4)"="$FR^{sng\\_bbr}_{t-h}\\mbox{After GFC}$",
    "frd_snggg_bbr:after_gfc"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_bbr, 1):l(after_gfc, 1)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_bbr, 2):l(after_gfc, 2)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_bbr, 3):l(after_gfc, 3)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_bbr, 4):l(after_gfc, 4)"="$FR^{snggg\\_bbr}_{t-h}\\mbox{After GFC}$",
    #bbr-interaction exclusions
    "frd_lg_bbr:frd_lg_exclusions_bbr"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "l(frd_lg_bbr, 1):l(frd_lg_exclusions_bbr, 1)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "l(frd_lg_bbr, 2):l(frd_lg_exclusions_bbr, 2)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "l(frd_lg_bbr, 3):l(frd_lg_exclusions_bbr, 3)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "l(frd_lg_bbr, 4):l(frd_lg_exclusions_bbr, 4)"="$FR^{lg\\_bbr}_{t-h}XExclusions$",
    "frd_rg_bbr:frd_rg_exclusions_bbr"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "l(frd_rg_bbr, 1):l(frd_rg_exclusions_bbr, 1)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "l(frd_rg_bbr, 2):l(frd_rg_exclusions_bbr, 2)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "l(frd_rg_bbr, 3):l(frd_rg_exclusions_bbr, 3)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "l(frd_rg_bbr, 4):l(frd_rg_exclusions_bbr, 4)"="$FR^{rg\\_bbr}_{t-h}XExclusions$",
    "frd_sng_bbr:frd_sng_exclusions_bbr"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "l(frd_sng_bbr, 1):l(frd_sng_exclusions_bbr, 1)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "l(frd_sng_bbr, 2):l(frd_sng_exclusions_bbr, 2)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "l(frd_sng_bbr, 3):l(frd_sng_exclusions_bbr, 3)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "l(frd_sng_bbr, 4):l(frd_sng_exclusions_bbr, 4)"="$FR^{sng\\_bbr}_{t-h}XExclusions$",
    "frd_snggg_bbr:frd_snggg_exclusions_bbr"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "l(frd_snggg_bbr, 1):l(frd_snggg_exclusions_bbr, 1)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "l(frd_snggg_bbr, 2):l(frd_snggg_exclusions_bbr, 2)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "l(frd_snggg_bbr, 3):l(frd_snggg_exclusions_bbr, 3)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    "l(frd_snggg_bbr, 4):l(frd_snggg_exclusions_bbr, 4)"="$FR^{snggg\\_bbr}_{t-h}XExclusions$",
    #dr
    "fit_frd_lg_dr"="$FR^{lg\\_dr}_{t-h}$",
    "fit_l(frd_lg_dr, 1)"="$FR^{lg\\_dr}_{t-h}$",
    "fit_l(frd_lg_dr, 2)"="$FR^{lg\\_dr}_{t-h}$",
    "fit_l(frd_lg_dr, 3)"="$FR^{lg\\_dr}_{t-h}$",
    "fit_l(frd_lg_dr, 4)"="$FR^{lg\\_dr}_{t-h}$",
    "fit_frd_rg_dr"="$FR^{rg\\_dr}_{t-h}$",
    "fit_l(frd_rg_dr, 1)"="$FR^{rg\\_dr}_{t-h}$",
    "fit_l(frd_rg_dr, 2)"="$FR^{rg\\_dr}_{t-h}$",
    "fit_l(frd_rg_dr, 3)"="$FR^{rg\\_dr}_{t-h}$",
    "fit_l(frd_rg_dr, 4)"="$FR^{rg\\_dr}_{t-h}$",
    "fit_frd_sng_dr"="$FR^{sng\\_dr}_{t-h}$",
    "fit_l(frd_sng_dr, 1)"="$FR^{sng\\_dr}_{t-h}$",
    "fit_l(frd_sng_dr, 2)"="$FR^{sng\\_dr}_{t-h}$",
    "fit_l(frd_sng_dr, 3)"="$FR^{sng\\_dr}_{t-h}$",
    "fit_l(frd_sng_dr, 4)"="$FR^{sng\\_dr}_{t-h}$",
    "fit_l(frd_sng_dr, 5)"="$FR^{sng\\_dr}_{t-h}$",
    "frd_lg_dr"="$FR^{lg\\_dr}_{t-h}$",
    "l(frd_lg_dr, 1)"="$FR^{lg\\_dr}_{t-h}$",
    "l(frd_lg_dr, 2)"="$FR^{lg\\_dr}_{t-h}$",
    "l(frd_lg_dr, 3)"="$FR^{lg\\_dr}_{t-h}$",
    "l(frd_lg_dr, 4)"="$FR^{lg\\_dr}_{t-h}$",
    "frd_rg_dr"="$FR^{rg\\_dr}_{t-h}$",
    "l(frd_rg_dr, 1)"="$FR^{rg\\_dr}_{t-h}$",
    "l(frd_rg_dr, 2)"="$FR^{rg\\_dr}_{t-h}$",
    "l(frd_rg_dr, 3)"="$FR^{rg\\_dr}_{t-h}$",
    "l(frd_rg_dr, 4)"="$FR^{rg\\_dr}_{t-h}$",
    "frd_sng_dr"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_sng_dr, 1)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_sng_dr, 2)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_sng_dr, 3)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_sng_dr, 4)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_sng_dr, 5)"="$FR^{sng\\_dr}_{t-h}$",
    "frd_snggg_dr"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_snggg_dr, 1)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_snggg_dr, 2)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_snggg_dr, 3)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_snggg_dr, 4)"="$FR^{sng\\_dr}_{t-h}$",
    "l(frd_snggg_dr, 5)"="$FR^{sng\\_dr}_{t-h}$",
    #dr-interaction gfc
    "frd_lg_dr:after_gfc"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_dr, 1):l(after_gfc, 1)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_dr, 2):l(after_gfc, 2)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_dr, 3):l(after_gfc, 3)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_dr, 4):l(after_gfc, 4)"="$FR^{lg\\_dr}_{t-h}\\mbox{After GFC}$",
    "frd_rg_dr:after_gfc"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_dr, 1):l(after_gfc, 1)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_dr, 2):l(after_gfc, 2)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_dr, 3):l(after_gfc, 3)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_dr, 4):l(after_gfc, 4)"="$FR^{rg\\_dr}_{t-h}\\mbox{After GFC}$",
    "frd_sng_dr:after_gfc"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_dr, 1):l(after_gfc, 1)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_dr, 2):l(after_gfc, 2)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_dr, 3):l(after_gfc, 3)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_dr, 4):l(after_gfc, 4)"="$FR^{sng\\_dr}_{t-h}\\mbox{After GFC}$",
    "frd_snggg_dr:after_gfc"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_dr, 1):l(after_gfc, 1)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_dr, 2):l(after_gfc, 2)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_dr, 3):l(after_gfc, 3)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_dr, 4):l(after_gfc, 4)"="$FR^{snggg\\_dr}_{t-h}\\mbox{After GFC}$",
    #dr-interaction
    "frd_lg_dr:frd_lg_exclusions_dr"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "l(frd_lg_dr, 1):l(frd_lg_exclusions_dr, 1)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "l(frd_lg_dr, 2):l(frd_lg_exclusions_dr, 2)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "l(frd_lg_dr, 3):l(frd_lg_exclusions_dr, 3)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "l(frd_lg_dr, 4):l(frd_lg_exclusions_dr, 4)"="$FR^{lg\\_dr}_{t-h}XExclusions$",
    "frd_rg_dr:frd_rg_exclusions_dr"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "l(frd_rg_dr, 1):l(frd_rg_exclusions_dr, 1)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "l(frd_rg_dr, 2):l(frd_rg_exclusions_dr, 2)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "l(frd_rg_dr, 3):l(frd_rg_exclusions_dr, 3)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "l(frd_rg_dr, 4):l(frd_rg_exclusions_dr, 4)"="$FR^{rg\\_dr}_{t-h}XExclusions$",
    "frd_sng_dr:frd_sng_exclusions_dr"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "l(frd_sng_dr, 1):l(frd_sng_exclusions_dr, 1)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "l(frd_sng_dr, 2):l(frd_sng_exclusions_dr, 2)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "l(frd_sng_dr, 3):l(frd_sng_exclusions_dr, 3)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "l(frd_sng_dr, 4):l(frd_sng_exclusions_dr, 4)"="$FR^{sng\\_dr}_{t-h}XExclusions$",
    "frd_snggg_dr:frd_snggg_exclusions_dr"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "l(frd_snggg_dr, 1):l(frd_snggg_exclusions_dr, 1)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "l(frd_snggg_dr, 2):l(frd_snggg_exclusions_dr, 2)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "l(frd_snggg_dr, 3):l(frd_snggg_exclusions_dr, 3)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    "l(frd_snggg_dr, 4):l(frd_snggg_exclusions_dr, 4)"="$FR^{snggg\\_dr}_{t-h}XExclusions$",
    #er
    "fit_frd_lg_er"="$FR^{lg\\_er}_{t-h}$",
    "fit_l(frd_lg_er, 1)"="$FR^{lg\\_er}_{t-h}$",
    "fit_l(frd_lg_er, 2)"="$FR^{lg\\_er}_{t-h}$",
    "fit_l(frd_lg_er, 3)"="$FR^{lg\\_er}_{t-h}$",
    "fit_l(frd_lg_er, 4)"="$FR^{lg\\_er}_{t-h}$",
    "fit_frd_rg_er"="$FR^{rg\\_er}_{t-h}$",
    "fit_l(frd_rg_er, 1)"="$FR^{rg\\_er}_{t-h}$",
    "fit_l(frd_rg_er, 2)"="$FR^{rg\\_er}_{t-h}$",
    "fit_l(frd_rg_er, 3)"="$FR^{rg\\_er}_{t-h}$",
    "fit_l(frd_rg_er, 4)"="$FR^{rg\\_er}_{t-h}$",
    "fit_frd_sng_er"="$FR^{sng\\_er}_{t-h}$",
    "fit_l(frd_sng_er, 1)"="$FR^{sng\\_er}_{t-h}$",
    "fit_l(frd_sng_er, 2)"="$FR^{sng\\_er}_{t-h}$",
    "fit_l(frd_sng_er, 3)"="$FR^{sng\\_er}_{t-h}$",
    "fit_l(frd_sng_er, 4)"="$FR^{sng\\_er}_{t-h}$",
    "fit_l(frd_sng_er, 5)"="$FR^{sng\\_er}_{t-h}$",
    "frd_lg_er"="$FR^{lg\\_er}_{t-h}$",
    "l(frd_lg_er, 1)"="$FR^{lg\\_er}_{t-h}$",
    "l(frd_lg_er, 2)"="$FR^{lg\\_er}_{t-h}$",
    "l(frd_lg_er, 3)"="$FR^{lg\\_er}_{t-h}$",
    "l(frd_lg_er, 4)"="$FR^{lg\\_er}_{t-h}$",
    "frd_rg_er"="$FR^{rg\\_er}_{t-h}$",
    "l(frd_rg_er, 1)"="$FR^{rg\\_er}_{t-h}$",
    "l(frd_rg_er, 2)"="$FR^{rg\\_er}_{t-h}$",
    "l(frd_rg_er, 3)"="$FR^{rg\\_er}_{t-h}$",
    "l(frd_rg_er, 4)"="$FR^{rg\\_er}_{t-h}$",
    "frd_sng_er"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_sng_er, 1)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_sng_er, 2)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_sng_er, 3)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_sng_er, 4)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_sng_er, 5)"="$FR^{sng\\_er}_{t-h}$",
    "frd_snggg_er"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_snggg_er, 1)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_snggg_er, 2)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_snggg_er, 3)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_snggg_er, 4)"="$FR^{sng\\_er}_{t-h}$",
    "l(frd_snggg_er, 5)"="$FR^{sng\\_er}_{t-h}$",
    #er-interaction gfc
    "frd_lg_er:after_gfc"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_er, 1):l(after_gfc, 1)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_er, 2):l(after_gfc, 2)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_er, 3):l(after_gfc, 3)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_lg_er, 4):l(after_gfc, 4)"="$FR^{lg\\_er}_{t-h}\\mbox{After GFC}$",
    "frd_rg_er:after_gfc"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_er, 1):l(after_gfc, 1)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_er, 2):l(after_gfc, 2)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_er, 3):l(after_gfc, 3)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_rg_er, 4):l(after_gfc, 4)"="$FR^{rg\\_er}_{t-h}\\mbox{After GFC}$",
    "frd_sng_er:after_gfc"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_er, 1):l(after_gfc, 1)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_er, 2):l(after_gfc, 2)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_er, 3):l(after_gfc, 3)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_sng_er, 4):l(after_gfc, 4)"="$FR^{sng\\_er}_{t-h}\\mbox{After GFC}$",
    "frd_snggg_er:after_gfc"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_er, 1):l(after_gfc, 1)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_er, 2):l(after_gfc, 2)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_er, 3):l(after_gfc, 3)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    "l(frd_snggg_er, 4):l(after_gfc, 4)"="$FR^{snggg\\_er}_{t-h}\\mbox{After GFC}$",
    #er-interaction
    "frd_lg_er:frd_lg_exclusions_er"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "l(frd_lg_er, 1):l(frd_lg_exclusions_er, 1)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "l(frd_lg_er, 2):l(frd_lg_exclusions_er, 2)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "l(frd_lg_er, 3):l(frd_lg_exclusions_er, 3)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "l(frd_lg_er, 4):l(frd_lg_exclusions_er, 4)"="$FR^{lg\\_er}_{t-h}XExclusions$",
    "frd_rg_er:frd_rg_exclusions_er"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "l(frd_rg_er, 1):l(frd_rg_exclusions_er, 1)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "l(frd_rg_er, 2):l(frd_rg_exclusions_er, 2)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "l(frd_rg_er, 3):l(frd_rg_exclusions_er, 3)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "l(frd_rg_er, 4):l(frd_rg_exclusions_er, 4)"="$FR^{rg\\_er}_{t-h}XExclusions$",
    "frd_sng_er:frd_sng_exclusions_er"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "l(frd_sng_er, 1):l(frd_sng_exclusions_er, 1)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "l(frd_sng_er, 2):l(frd_sng_exclusions_er, 2)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "l(frd_sng_er, 3):l(frd_sng_exclusions_er, 3)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "l(frd_sng_er, 4):l(frd_sng_exclusions_er, 4)"="$FR^{sng\\_er}_{t-h}XExclusions$",
    "frd_snggg_er:frd_snggg_exclusions_er"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "l(frd_snggg_er, 1):l(frd_snggg_exclusions_er, 1)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "l(frd_snggg_er, 2):l(frd_snggg_exclusions_er, 2)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "l(frd_snggg_er, 3):l(frd_snggg_exclusions_er, 3)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    "l(frd_snggg_er, 4):l(frd_snggg_exclusions_er, 4)"="$FR^{snggg\\_er}_{t-h}XExclusions$",
    #gfcf_nms_pps
    "l(log(ardeco_gfcf_nms_pps), 1)"="$\\mbox{dep var}_{t-1}$",
    "l(log(ardeco_gfcf_nms_pps), 2)"="$\\mbox{dep var}_{t-2}$",
    "l(log(ardeco_gfcf_nms_pps), 3)"="$\\mbox{dep var}_{t-3}$",
    "l(log(ardeco_gfcf_nms_pps), 4)"="$\\mbox{dep var}_{t-4}$",
    "l(log(ardeco_gfcf_nms_pps), 5)"="$\\mbox{dep var}_{t-5}$",
    "l(log(ardeco_gfcf_nms_pps), 6)"="$\\mbox{dep var}_{t-6}$",
    "l(log(ardeco_gfcf_nms_pps), 7)"="$\\mbox{dep var}_{t-7}$",
    "l(log(ardeco_gfcf_nms_pps), 8)"="$\\mbox{dep var}_{t-8}$",

    #gva_nms_pps
    "l(log(ardeco_gva_nms_pps), 1)"="$\\mbox{dep var}_{t-1}$",
    "l(log(ardeco_gva_nms_pps), 2)"="$\\mbox{dep var}_{t-2}$",
    "l(log(ardeco_gva_nms_pps), 3)"="$\\mbox{dep var}_{t-3}$",
    "l(log(ardeco_gva_nms_pps), 4)"="$\\mbox{dep var}_{t-4}$",
    "l(log(ardeco_gva_nms_pps), 5)"="$\\mbox{dep var}_{t-5}$",
    "l(log(ardeco_gva_nms_pps), 6)"="$\\mbox{dep var}_{t-6}$",
    "l(log(ardeco_gva_nms_pps), 7)"="$\\mbox{dep var}_{t-7}$",
    "l(log(ardeco_gva_nms_pps), 8)"="$\\mbox{dep var}_{t-8}$",

    #gva_nms_cp
    "l(log(ardeco_gva_nms_cp), 1)"="$\\mbox{dep var}_{t-1}$",
    "l(log(ardeco_gva_nms_cp), 2)"="$\\mbox{dep var}_{t-2}$",
    "l(log(ardeco_gva_nms_cp), 3)"="$\\mbox{dep var}_{t-3}$",
    "l(log(ardeco_gva_nms_cp), 4)"="$\\mbox{dep var}_{t-4}$",
    "l(log(ardeco_gva_nms_cp), 5)"="$\\mbox{dep var}_{t-5}$",
    "l(log(ardeco_gva_nms_cp), 6)"="$\\mbox{dep var}_{t-6}$",
    "l(log(ardeco_gva_nms_cp), 7)"="$\\mbox{dep var}_{t-7}$",
    "l(log(ardeco_gva_nms_cp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gva_nms_gdp
    "l(log(ardeco_gva_gdp), 1)"="$\\mbox{dep var}_{t-1}$",
    "l(log(ardeco_gva_gdp), 2)"="$\\mbox{dep var}_{t-2}$",
    "l(log(ardeco_gva_gdp), 3)"="$\\mbox{dep var}_{t-3}$",
    "l(log(ardeco_gva_gdp), 4)"="$\\mbox{dep var}_{t-4}$",
    "l(log(ardeco_gva_gdp), 5)"="$\\mbox{dep var}_{t-5}$",
    "l(log(ardeco_gva_gdp), 6)"="$\\mbox{dep var}_{t-6}$",
    "l(log(ardeco_gva_gdp), 7)"="$\\mbox{dep var}_{t-7}$",
    "l(log(ardeco_gva_gdp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gfcf_nms_cp
    "l(log(ardeco_gfcf_nms_cp), 1)"="$\\mbox{dep var}_{t-1}$",
    "l(log(ardeco_gfcf_nms_cp), 2)"="$\\mbox{dep var}_{t-2}$",
    "l(log(ardeco_gfcf_nms_cp), 3)"="$\\mbox{dep var}_{t-3}$",
    "l(log(ardeco_gfcf_nms_cp), 4)"="$\\mbox{dep var}_{t-4}$",
    "l(log(ardeco_gfcf_nms_cp), 5)"="$\\mbox{dep var}_{t-5}$",
    "l(log(ardeco_gfcf_nms_cp), 6)"="$\\mbox{dep var}_{t-6}$",
    "l(log(ardeco_gfcf_nms_cp), 7)"="$\\mbox{dep var}_{t-7}$",
    "l(log(ardeco_gfcf_nms_cp), 8)"="$\\mbox{dep var}_{t-8}$",

    #gfcf_nms_gdp
    "l(log(ardeco_gfcf_gdp), 1)"="$\\mbox{dep var}_{t-1}$",
    "l(log(ardeco_gfcf_gdp), 2)"="$\\mbox{dep var}_{t-2}$",
    "l(log(ardeco_gfcf_gdp), 3)"="$\\mbox{dep var}_{t-3}$",
    "l(log(ardeco_gfcf_gdp), 4)"="$\\mbox{dep var}_{t-4}$",
    "l(log(ardeco_gfcf_gdp), 5)"="$\\mbox{dep var}_{t-5}$",
    "l(log(ardeco_gfcf_gdp), 6)"="$\\mbox{dep var}_{t-6}$",
    "l(log(ardeco_gfcf_gdp), 7)"="$\\mbox{dep var}_{t-7}$",
    "l(log(ardeco_gfcf_gdp), 8)"="$\\mbox{dep var}_{t-8}$",


    #gfcf_gva_ratio
    "l(ardeco_gfcf_gva_ratio, 1)"="$\\mbox{dep var}_{t-1}$",
    "l(ardeco_gfcf_gva_ratio, 2)"="$\\mbox{dep var}_{t-2}$",
    "l(ardeco_gfcf_gva_ratio, 3)"="$\\mbox{dep var}_{t-3}$",
    "l(ardeco_gfcf_gva_ratio, 4)"="$\\mbox{dep var}_{t-4}$",
    "l(ardeco_gfcf_gva_ratio, 5)"="$\\mbox{dep var}_{t-5}$",
    "l(ardeco_gfcf_gva_ratio, 6)"="$\\mbox{dep var}_{t-6}$",
    "l(ardeco_gfcf_gva_ratio, 7)"="$\\mbox{dep var}_{t-7}$",
    "l(ardeco_gfcf_gva_ratio, 8)"="$\\mbox{dep var}_{t-8}$"

  )
}

#define header
  max_lag <- max(lag_order)
  start_values <- seq(from = 1, by = max_lag + 1, length.out = length(headers))
  end_values <- start_values + max_lag
  header_list <- setNames(lapply(seq_along(headers), function(i) start_values[i]:end_values[i]), headers)

  texreg(l=l,
         file = paste0(path,filename,".tex"),
         fontsize = fontsize,
         scalebox = scalebox,
         stars = c( 0.01, 0.05,0.1),
         override.se=se,
         override.pvalues=p,
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
         float.pos = "H",
         digits = digits,
         custom.model.names = paste0("h=",rep(lag_order,length(headers))),
         custom.header = header_list,
         caption = caption,
         leading.zero = FALSE
  )

}
