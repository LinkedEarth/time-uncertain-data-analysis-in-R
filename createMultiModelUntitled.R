createMultiModelEnsemble <- function(L,
                                     depth.var = "depth",
                                     age.var = "ageEnsemble",
                                     models.to.combine,
                                     chron.num = 1, 
                                     depth.interval =10 ,
                                     depth.seq = NA,
                                     ens.table.number = 1,
                                     n.ens = 1000){
  
  if(is.na(depth.seq)){
  #figure out the depth sequence
  depthrange <- function(mod,depth.var,ens.table.number){
    return(mod$ensembleTable[[ens.table.number]][[depth.var]]$values %>% 
    range())
  }
  
  ranges <- suppressMessages(purrr::map_dfc(L$chronData[[chron.num]]$model[models.to.combine],depthrange,depth.var,ens.table.number) )
    
  ds <- max(ranges[1,],na.rm = TRUE)
  de <- min(ranges[2,],na.rm = TRUE)
  
  depth.seq <- seq(ds,de,by = depth.interval)
  }
  
  ##interpolate ensemble tables
  interpolateEnsembles <- function(mod,
                                   depth.seq,
                                   depth.var,
                                   age.var,
                                   ens.table.number,
                                   n.ens.out){
    
    od <- mod$ensembleTable[[ens.table.number]][[depth.var]]$values
    oa <- mod$ensembleTable[[ens.table.number]][[age.var]]$values %>% purrr::array_branch(margin = 2)
    ia <- purrr::map_dfc(oa,~ approx(od,.x,depth.seq)$y)
    neo <- nrow(ia)
    if(neo >= n.ens.out){
      replace <- FALSE
    }else{
      replace <- TRUE
    }
    return(ia[ , sample.int(neo,size = n.ens.out,replace = replace)])
  }
  
  interpolatedEnsembles <- suppressMessages(purrr::map_dfc(L$chronData[[chron.num]]$model[models.to.combine],
                                          interpolateEnsembles,
                                          depth.seq,
                                          depth.var,
                                          age.var,
                                          ens.table.number,
                                          ceiling(n.ens/length(models.to.combine)))) %>% 
    as.matrix()
    
   to.output <- interpolatedEnsembles[,sample.int(ncol(interpolatedEnsembles),n.ens,replace = FALSE)]
   
   #make some methods.
   
   allMethods <- purrr::map(L$chronData[[chron.num]]$model[models.to.combine],purrr::pluck,"methods")
   methods <- list(algorithm = "grand model ensemble by geoChronR::createMultiModelEnsemble()", originalMethods = allMethods)
   
    L <- createModel(L,
                    depth.or.age.vector = depth.seq,
                    ensemble.data = to.output,
                    model.num = length(L$chronData[[chron.num]]$model) +1,
                    paleo.or.chron = "chronData",
                    paleo.or.chron.num = chron.num,
                    depth.or.age.var = depth.var,
                    depth.or.age.units = L$chronData[[chron.num]]$model[[models.to.combine[1]]]$ensembleTable[[ens.table.number]][[depth.var]]$units,
                    ens.var = age.var,
                    L$chronData[[chron.num]]$model[[models.to.combine[1]]]$ensembleTable[[ens.table.number]][[age.var]]$units,
                    make.new = TRUE,
                    create.summary.table = TRUE,
                    methods = methods)
                    
                    
                    
    return(L)
                    
     
  
}

