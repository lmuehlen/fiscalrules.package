get_effect_plot<-function(data,filename,scale_x=TRUE,facet_var_x=NULL,facet_order_x=NULL,facet_var_y=NULL,facet_order_y=NULL,font="serif",color_palette=NULL,no_fill_legend=FALSE,size=25,height=15,width=25,limits=NULL,breaks=NULL,path="C:/Users/leona/Dropbox/Apps/Overleaf/The Political Economy of Fiscal Rules - Between Deficit and Disinvestment Bias (Mai 2023)/Graphics/"){

  if(!is.null(facet_var_x)&is.null(facet_order_x)){
    data[[facet_var_x]] <- factor(data[[facet_var_x]], levels = data[[facet_var_x]]%>%unique())
  }else if(!is.null(facet_var_x)&!is.null(facet_order_x)){
    data[[facet_var_x]] <- factor(data[[facet_var_x]], levels = facet_order_x)
  }

  if(!is.null(facet_var_y)&is.null(facet_order_y)){
    data[[facet_var_y]] <- factor(data[[facet_var_y]], levels = data[[facet_var_y]]%>%unique())
  }else if(!is.null(facet_var_y)&!is.null(facet_order_y)){
    data[[facet_var_y]] <- factor(data[[facet_var_y]], levels = facet_order_y)
  }

  if(!is.null(facet_var_x)&is.null(facet_var_y)){
    facet_formula<-as.formula(paste("~", facet_var_x))
  }else if(!is.null(facet_var_x)&!is.null(facet_var_y)){
    facet_formula<-as.formula(paste(facet_var_y,"~", facet_var_x))
  }


if(is.null(color_palette)){
  color_palette<-c("#ff6555","#000e8e")
}

if(no_fill_legend==FALSE){
  guide=guide_legend(override.aes = list(size=1,shape=15,linetype=0))
  just=c(1.3,2)
}else{
  guide="none"
  just="center"
}


  min<-floor(min(data$conf.low)*100)/100
  max<-ceiling(max(data$conf.high)*100)/100


  if(is.null(breaks)){
    breaks=seq(from=min,to=max,by=0.01)
  }

  if(is.null(limits)){
    limits=c(min,max)
  }

  p<-data%>%
    ggplot()+
    geom_pointrange(aes(y=term,x=estimate,xmin=conf.low,xmax=conf.high,fill=dep_var,color=dep_var,shape=upper_lower),position=position_dodge(width=0.6),fatten=4)+
    geom_vline(xintercept = 0,linetype="dashed")+
    scale_x_continuous(breaks = breaks,limits = limits)+
    {if(scale_x==TRUE) scale_x_continuous(breaks = breaks,limits = limits,labels = function(x) paste0(x*100,"%"))}+
    scale_y_discrete(limits=rev)+theme_tufte(base_family=font,base_size=size,ticks = FALSE)+
    theme(
          legend.position = "bottom",
          legend.justification = just)+
    labs(x=NULL,y=NULL,color=NULL,shape=NULL)+
    scale_color_manual(values = color_palette,guide=guide)+
    scale_fill_manual(values = color_palette)+
    scale_shape_manual(values=c(21,24),guide=guide_legend(override.aes = list(size=1,fill="black",color="black",linetype=0)))+
    guides(fill="none")

  if(!is.null(facet_var_x)){
    p<-p+facet_grid(facet_formula)
  }


  ggsave(filename,p,device = "pdf",height=height,width=width,units="cm",path = path)
return(p)
}
