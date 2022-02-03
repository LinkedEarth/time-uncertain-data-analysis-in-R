selectData = function(L,
                      var.name=NA,
                      paleo.or.chron="paleoData",
                      paleo.or.chron.num=NA,
                      table.type = "measurement",
                      meas.table.num=NA,
                      always.choose=FALSE,
                      alt.names=NA,
                      model.num = 1,
                      ens.table.num=1,
                      sum.table.num = 1,
                      strict.search = FALSE){
  #paleo or chron
  P = L[[paleo.or.chron]]
  
  #which <paleo.or.chron>
  
  if(is.na(paleo.or.chron.num)){
    if(length(P)==1){
      paleo.or.chron.num=1
    }else{
      print(names(P))
      paleo.or.chron.num=as.integer(readline(prompt = "Which do you want? Select a number "))
    }
  }
  
  if(is.na(table.type)){
    table.type=readline(prompt = "Do you want a variable from a measurementTable (m), model summaryTable (s), or model ensembleTable (e)?")
  }
  
  if(tolower(substr(table.type,1,1))=="m"){
    MT = P[[paleo.or.chron.num]]$measurementTable
  }else{#check on model.num
    if(is.na(model.num)){
      if(length(P[[paleo.or.chron.num]]$model)==1){
        model.num=1
      }else{
        print(paste0("There are ",length(P[[paleo.or.chron.num]]$model)," models. Which do you want?"))
        model.num=as.integer(readline(prompt = "Which model do you want? Select a number "))
      }
    }
  }
  
  
  
  if(tolower(substr(table.type,1,1))=="e"){
    MTD = P[[paleo.or.chron.num]]$model[[model.num]]$ensembleTable[[ens.table.num]]
  }else if(tolower(substr(table.type,1,1))=="s"){
    MTD = P[[paleo.or.chron.num]]$model[[model.num]]$summaryTable[[1]]
  }else{ #measurementTable
    #initialize table number
    if(is.na(meas.table.num)){
      if(length(MT)==0){
        stop(paste0("this object in ",paleo.or.chron,"[[",as.character(paleo.or.chron.num),"]] has ", as.character(length(MT)), " tables"))
      }
      if(length(MT)==1){
        #only one pmt
        meas.table.num=1
      }else{
        print(paste0("this object in ",paleo.or.chron,"[[",as.character(paleo.or.chron.num),"]] has ", as.character(length(MT)), " tables"))
        meas.table.num=as.integer(readline(prompt = "Which table do you want? Enter an integer "))
      }
    }
    
    
    #this is the table of interest  
    MTD=MT[[meas.table.num]]
  }
  
  ind = getVariableIndex(MTD,var.name = var.name,always.choose = always.choose,alt.names = alt.names,strict.search = strict.search)
  
  varList = MTD[[ind]]
  
  return(varList)
  
  
  
}

mapAgeEnsembleToPaleoData = function(L,
                                     age.var = "ageEnsemble",
                                     chron.depth.var = "depth",
                                     paleo.depth.var = "depth",
                                     paleo.num=NA,
                                     paleo.meas.table.num=NA,
                                     chron.num=NA,
                                     model.num=NA,
                                     ens.table.num = 1,
                                     max.ens=NA,
                                     strict.search=FALSE){
  print(L$dataSetName)
  #check on the model first
  if(is.null(L$chronData)){
    stop("There's no chronData in this file")
  }
  
  #initialize chron.num
  if(is.na(chron.num)){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to pull this ensemble from? "))
    }
  }
  
  #initialize model number
  if(length(L$chronData[[chron.num]]$model)==0){
    stop("No model in this chronData")
  }
  if(is.na(model.num)){
    if(length(L$chronData[[chron.num]]$model)==1){
      #only one model
      model.num=1
    }else{
      print(paste("ChronData", chron.num, "has", length(L$chronData[[chron.num]]$model), "models"))
      model.num=as.integer(readline(prompt = "Which chron model do you want to get the ensemble from? Enter an integer "))
    }
  }
  
  
  #initialize paleo.num
  if(is.na(paleo.num)){
    if(length(L$paleoData)==1){
      paleo.num=1
    }else{
      paleo.num=as.integer(readline(prompt = "Which paleoData do you want to put this age ensemble in? "))
    }
  }
  
  #initialize measurement table number
  if(is.na(paleo.meas.table.num)){
    if(length(L$paleoData[[paleo.num]]$measurementTable)==1){
      #only one pmt
      paleo.meas.table.num=1
    }else{
      print(paste("PaleoData", paleo.num, "has", length(L$paleoData[[paleo.num]]$measurementTable), "measurement tables"))
      paleo.meas.table.num=as.integer(readline(prompt = "Which measurement table do you want to put the ensemble in? Enter an integer "))
    }
  }
  
  
  #make sure the ensemble is there, with data
  copyAE  = FALSE
  
  print("Looking for age ensemble....")
  ensDepth = selectData(L,
                        table.type = "ensemble",
                        var.name = chron.depth.var,
                        paleo.or.chron = "chronData",
                        paleo.or.chron.num = chron.num,
                        ens.table.num = ens.table.num,
                        strict.search = strict.search,
                        model.num = model.num)$values
  
  ensAll = selectData(L,
                      table.type = "ensemble",
                      var.name = age.var,
                      paleo.or.chron = "chronData",
                      model.num = model.num,
                      ens.table.num = ens.table.num,
                      paleo.or.chron.num = chron.num,
                      strict.search = strict.search)
  
  if(is.null(ensAll$values)){
    stop("Error: did not find the age ensemble.")
  }
  
  ens = ensAll$values
  
  if(is.null(ensDepth)){#if there are no depth data in the ensemble, try to apply the ensemble straight in (no interpolation)
    #check for the same size
    #get year, age or depth from paleodata
    pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = "year",always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
    if(is.null(pdya)){
      pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = "age",always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
    }
    if(is.null(pdya)){
      pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = age.var,always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
    }
    if(is.null(pdya)){
      pdya = selectData(L,paleo.or.chron.num = paleo.num,var.name = depth.var,always.choose = FALSE,ens.table.num = ens.table.num,strict.search = strict.search,meas.table.num = paleo.meas.table.num)$values
    }
    if(is.null(pdya)){
      stop("Couldnt find depth in the ensembleTable, or year, age or depth in the paleoTable. I need more help from you.")    
    }
    
    #check for length of that variable
    if(length(pdya)  == nrow(ens)){
      copyAE  = TRUE
    }else{
      stop("Couldnt find depth in the ensembleTable, and the paleoData measurementTable has a different number of rows thant the ensemble.")    
    }
  }
  
  
  if(!copyAE){
    #get the depth from the paleo measurement table
    print("getting depth from the paleodata table...")
    depth = selectData(L,paleo.or.chron.num = paleo.num,var.name = chron.depth.var,always.choose = FALSE,ens.table.num = ens.table.num,meas.table.num = paleo.meas.table.num)$values
    
    #check that depth is numeric
    if(!is.numeric(depth)){
      stop("Uh oh, paleo depth is not a numeric vector. That will cause problems - check paleoData[[p]]measurementTable[[m]]$depth$values (or similar if var.name is not depth)")
    }
    
    #restrict ensemble members
    if(!is.na(max.ens)){
      if(ncol(ens)>max.ens){
        #randomly select the appropriate number of ensemble members
        ens = ens[,sample.int(ncol(ens),size = max.ens,replace = F)]
      }
    }
    
    #interpolate
    na.depth.i = which(!is.na(depth))
    aei = matrix(nrow = length(depth),ncol = ncol(ens))
    aeig=pbapply::pbapply(X=ens,MARGIN = 2,FUN = function(y) Hmisc::approxExtrap(ensDepth,y,xout=depth[na.depth.i],na.rm=TRUE)$y)
    aei[na.depth.i,] = aeig
    
    
  }else{
    #check to see if the ensemble needs to be flipped
    #correlate pdya with ens[,1]
    
    test.cor <- cor(pdya,ens[,1])
    if(test.cor < 0){
      aei <- apply(ens,2,rev)
    }else{
      aei = ens
    }
  }
  
  #guess
  if(is.na(ens.table.num)){ens.table.num=1}
  
  
  #assign into measurementTable
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$variableName = ensAll$variableName
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$values = aei
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$units = ensAll$units
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$fromChronData = chron.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$frommodel = model.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]]$ageEnsemble$description = paste("age ensemble pulled from chronData", chron.num,"model",model.num,"- fit to paleoData depth with linear interpolation")
  
  
  return(L)
  
  
}
