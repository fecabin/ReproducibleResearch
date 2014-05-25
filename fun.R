deliverActualDmgValue1<-function (dd){
  unit<- c("B","M","K","H")
  
  multi<-c(1000000000,1000000,1000,100)
  
  dd$corpdmgvalue<-dd$corpdmg
  dd$propdmgvalue<-dd$propdmg
  for ( i in  1:length(unit)){
    indexCrodFit<-grep(unit[i],toupper(dd[ ,"corpdmgexp"]))
    if(length(indexCrodFit)>0){
      dd[indexCrodFit,'corpdmgvalue']<- multi[i]*dd[indexCrodFit ,"corpdmg"]
    }
    indexProdFit<-grep(unit[i],toupper(dd[ ,"propdmgexp"]))
    if(length(indexProdFit)>0){
      dd[indexProdFit,'propdmgvalue']<- multi[i]* dd[indexProdFit ,"propdmg"]
    }
  }
  return (dd)
}

deliverEvtUnifiedName2<-function (dd){
  evtMatcher<-data.frame( reg=c("NADO|FUNNEL|WATERSPOUT","THUNDER|STORM|WIND","HAIL"
                                ,"BLIZZARD|WINTER|COLD|LOW TEMP|RECORD LOW|SNOW|ICE","HEAT|WARM|RECORD HIGH","COSTAL STORM","SUNAMI","RIP CURRENT"
                                ,"FREEZ|FROST|FLASH FLOOD|FLD|FLOOD","RIVER FLOOD|URBAN FLOOD","TROPICAL STORM|TROPICAL","HURRICANE"
                                ,"DROUGHT","DUST STORM","DUST DEVIL","RAIN","LIGHTNING") )
  
  factorId<-c("Tornado","Thunderstorm wind","Hail"
                        ,"Cold","Heat","Costal Storm","Sunami","Rip current"
                        ,"Flash flood","River flood","Tropical Storm","Hurricane"
                        ,"Drought","Dust storm","Dust devil","Rain","Ligntning","Other");
 
  for ( i in  1:nrow(evtMatcher)){
    indexFit<-grep(evtMatcher[i,"reg"],toupper(dd[ ,"evtype"]))
    if(length(indexFit)>0){
      print(factorId[i]) 
      print(length(indexFit)) 
      
      dd[indexFit,"event"]<- as.character(factorId[i])
     
    }
  }

  return (dd)
}