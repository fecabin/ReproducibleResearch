# Analysis on storm aganistpopulation healh



## 1- Synopsis
  - This paper presents an analysis about the effect of storm on the population health.[Why we study this topic]. We analysis the data published by the )National Weather Service Instruction in 2007. These data . We use [data process technic]. We found[research findins] . [Implication and advice]


<<<<<<< HEAD
## 2 - Analysis Question
 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to popuulation health
 2.  Across the United States, which types of events have the greatest economic consequences
 
## 3- Data Processing

- 1. Load data

```r
# attributes dUrl <-
# 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
# download.file(dUrl, f, mode = 'wb')
ff <- file.path(getwd(), "repdata-data-StormData.csv.bz2")
data <- read.csv(ff, stringsAsFactors = FALSE, quote = "")
```


- 2. Variables we keep for analysis
  Variabl Name | Description|
  -------------|-------------|
   state      | Abbervation for states in US |
   evtype     | Event type |
   fatalites  | The number of people died|
   injures   | The number of people injuured|
   propdmg    | The amount of property damge (measured in money)|
   propdmgexp  | The unit of  damge (B,M,K,H)|
   corpdmg  | The amount of corp damge (measured in money)|
   corpdmgexp    | The unit of  damge (B,M,K,H)|
  

```r

# attributes<-c('X.STATE.','X.EVTYPE.','X.FATALITIES.','X.INJURIES.','X.PROPDMG.','X.PROPDMGEXP.','X.CROPDMG##.','X.CROPDMGEXP.')
attributes <- c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
    "CROPDMG", "CROPDMGEXP")
pData <- data[, attributes]
```

```
## Error: undefined columns selected
```

```r
colnames(pData) <- c("state", "evtype", "fatalites", "injures", "propdmg", "propdmgexp", 
    "corpdmg", "corpdmgexp")
```

### 3.1 Data cleanning
   
#### 3.1.1  Event tpye 
      - Create a column as the unified name of that eventype 
      - The hierachy looks like as follow:
        - Convection
          - lightning
          - tornado
          - thunderstorm Wind
          - hail
        - ExtremeTemp
          - cold
          - heat
        - Marine
          - costal Storm
          - sunami
          - rip Current
        - Flood
          - flash Flood
          - river Flood
        - Tropical Cyclones
          - tropical Storm
          - hurricane
        - Other
          - drought
          - dust Storm
          - dust Devil
          - rain
      - The unified event name and filtr expression:
 |  Event    |    Filter expression  |  
------------------| ----------------|
  Lightning |  LIGHTNING | 
  Tornado   | NADO,FUNNEL,WATERSPOUT"|
  Thunderstorm wind |  THUNDER,STORM,WIND | 
  Hail |  HAIL | 
  Cold |  BLIZZARD|WINTER|COLD|LOW TEMP|RECORD LOW|SNOW|ICE | 
  Heat |  HEAT|WARM|RECORD HIGH | 
  Costa l Storm |  COSTAL STORM | 
  Sunami | SUNAMI | 
  Rip current|RIP CURRENT
  Flash flood   |  FLASH FLOOD,FLD,FLOOD | 
  River flood   |  RIVER FLOOD , URBAN FLOOD | 
  Tropical Storm |TROPICAL STORM,TROPICAL|
  Hurricane   | HURRICANE |
   Drought   | DROUGHT |
    Dust storm   | DUST STORM |
        Dust devil   | DUST DEVIL |
    RAIN  | rain |



```r
deliverEvtUnifiedName <- function(dd) {
    evtMatcher <- data.frame(reg = c("NADO|FUNNEL|WATERSPOUT", "THUNDER|STORM|WIND", 
        "HAIL", "FROST|FREEZ|BLIZZARD|WINTER|COLD|LOW TEMP|RECORD LOW|SNOW|ICE", 
        "HEAT|WARM|RECORD HIGH", "COSTAL STORM", "SUNAMI", "RIP CURRENT", "FLASH FLOOD|FLD|FLOOD", 
        "RIVER FLOOD|URBAN FLOOD", "TROPICAL STORM|TROPICAL", " ", "DROUGHT", 
        "DUST STORM", "DUST DEVIL", "RAIN", "LIGHTNING"))
    
    factorId <- c("Tornado", "Thunderstorm wind", "Hail", "Cold", "Heat", "Costal Storm", 
        "Sunami", "Rip current", "Flash flood", "River flood", "Tropical Storm", 
        "Hurricane", "Drought", "Dust storm", "Dust devil", "Rain", "Ligntning", 
        "Other")
    
    for (i in 1:nrow(evtMatcher)) {
        indexFit <- grep(evtMatcher[i, "reg"], toupper(dd[, "evtype"]))
        if (length(indexFit) > 0) {
            dd[indexFit, "event"] <- as.character(factorId[i])
            
        }
    }
}
```


  - Use the function to filter and put value into "event"

```r


pData$event <- ("-")

# Give each event an unified id factor (17+1 factors)
pData <- deliverEvtUnifiedName(pData)
otherIndex <- grep("-", pData[, "event"])
pData[otherIndex, "event"] <- "Other"
```

```
## Error: 矩陣中的下標數目不正確
```

```r
pData$event <- as.factor(pData$event)

# See the event and number of that
table(sort(-pData$event))
```

```
## Warning: - not meaningful for factors
```

```
## < table of extent 0 >
```


#### 3.1.2  Create a variable has exact amount for property and corp damage
        - Fomula I  : propdmgValue  = propdmg       *  propdmgexp
        - Fomula II : corpdmgValue  = cropdmg       *  cropdmgexp
        - New Variable: ecodmgvalue = propdmgValue+corpdmgValue
        
              propdmgexp   |  unit   
           ----------------| ----------------
              B            |  1,000,000,000
              M            |      1,000,000   
              K            |          1,000 
              H            |            100
              NA or BLANK  |              1


```r
deliverActualDmgValue <- function(dd) {
    unit <- data.frame(cha = c("B", "M", "K", "H"), val = c(1e+09, 1e+06, 1000, 
        100))
    dd$cropdmgvalue <- as.numeric(dd$corpdmg)
    dd$propdmgvalue <- as.numeric(dd$propdmg)
    for (i in 1:nrow(unit)) {
        indexCrodFit <- grep(as.character(unit[i, "cha"]), toupper(dd[, "corpdmgexp"]))
        if (length(indexCrodFit) > 0) {
            dd[indexCrodFit, "corpdmgvalue"] <- unit[i, "val"] * dd[indexCrodFit, 
                "corpdmg"]
        }
        indexProdFit <- grep(as.character(unit[i, "cha"]), toupper(dd[, "propdmgexp"]))
        if (length(indexProdFit) > 0) {
            dd[indexProdFit, "propdmgvalue"] <- unit[i, "val"] * dd[indexProdFit, 
                "propdmg"]
        }
    }
    return(dd)
}

pData <- deliverActualDmgValue(pData)
```

```
## Error: 維度數目不正確
```

```r

# see the value:
summary(pData$corpdmgvalue)
```

```
## Length  Class   Mode 
##      0   NULL   NULL
```

```r
summary(pData$prodmgvalue)
```

```
## Length  Class   Mode 
##      0   NULL   NULL
```

```r


# Add a new variable: propdmagevalue + corpdmgvalue
pData$ecodmgvalue <- pData$corpdmgvalue + pData$propdmgvalue
pData$pephealthdmg <- pData$injures + pData$fatalites
summary(pData$ecodmgvalue)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
```

          
#### 3.2 Data Aggeratiion

```r

t <- aggregate(cbind(propdmgvalue, corpdmgvalue, injures, fatalites) ~ event, 
    pData, FUN = sum)
```

```
## Error: 找不到物件 'propdmgvalue'
```

```r

tidy <- melt(t, id.var = "event", variable.name = "variable")
```

```
## Error: 沒有這個函數 "melt"
```

```r
print(tidy)
```

```
##                event   damagetype     value
## 1               Cold propdmgvalue 1.266e+10
## 2            Drought propdmgvalue 1.046e+09
## 3         Dust devil propdmgvalue 7.191e+05
## 4         Dust storm propdmgvalue 5.599e+06
## 5        Flash flood propdmgvalue 1.623e+11
## 6               Hail propdmgvalue 1.762e+10
## 7               Heat propdmgvalue 2.013e+07
## 8          Hurricane propdmgvalue 8.476e+10
## 9          Ligntning propdmgvalue 9.390e+08
## 10             Other propdmgvalue 9.578e+09
## 11              Rain propdmgvalue 3.265e+09
## 12       Rip current propdmgvalue 1.630e+05
## 13       River flood propdmgvalue 5.260e+09
## 14            Sunami propdmgvalue 1.441e+08
## 15 Thunderstorm wind propdmgvalue 6.497e+10
## 16           Tornado propdmgvalue 5.700e+10
## 17    Tropical Storm propdmgvalue 7.716e+09
## 18              Cold corpdmgvalue 6.733e+09
## 19           Drought corpdmgvalue 1.397e+10
## 20        Dust devil corpdmgvalue 0.000e+00
## 21        Dust storm corpdmgvalue 3.600e+06
## 22       Flash flood corpdmgvalue 9.214e+09
## 23              Hail corpdmgvalue 3.114e+09
## 24              Heat corpdmgvalue 9.044e+08
## 25         Hurricane corpdmgvalue 5.515e+09
## 26         Ligntning corpdmgvalue 1.210e+07
## 27             Other corpdmgvalue 5.722e+08
## 28              Rain corpdmgvalue 9.193e+08
## 29       Rip current corpdmgvalue 0.000e+00
## 30       River flood corpdmgvalue 5.059e+09
## 31            Sunami corpdmgvalue 2.000e+04
## 32 Thunderstorm wind corpdmgvalue 1.975e+09
## 33           Tornado corpdmgvalue 4.150e+08
## 34    Tropical Storm corpdmgvalue 6.949e+08
## 35              Cold      injures 6.332e+03
## 36           Drought      injures 1.900e+01
## 37        Dust devil      injures 4.300e+01
## 38        Dust storm      injures 4.400e+02
## 39       Flash flood      injures 8.698e+03
## 40              Hail      injures 1.467e+03
## 41              Heat      injures 9.228e+03
## 42         Hurricane      injures 1.328e+03
## 43         Ligntning      injures 5.232e+03
## 44             Other      injures 3.565e+03
## 45              Rain      injures 3.050e+02
## 46       Rip current      injures 5.290e+02
## 47       River flood      injures 3.000e+00
## 48            Sunami      injures 1.290e+02
## 49 Thunderstorm wind      injures 1.139e+04
## 50           Tornado      injures 9.144e+04
## 51    Tropical Storm      injures 3.830e+02
## 52              Cold    fatalites 1.083e+03
## 53           Drought    fatalites 6.000e+00
## 54        Dust devil    fatalites 2.000e+00
## 55        Dust storm    fatalites 2.200e+01
## 56       Flash flood    fatalites 1.552e+03
## 57              Hail    fatalites 4.500e+01
## 58              Heat    fatalites 3.172e+03
## 59         Hurricane    fatalites 1.350e+02
## 60         Ligntning    fatalites 8.170e+02
## 61             Other    fatalites 6.570e+02
## 62              Rain    fatalites 1.140e+02
## 63       Rip current    fatalites 5.770e+02
## 64       River flood    fatalites 5.000e+00
## 65            Sunami    fatalites 3.300e+01
## 66 Thunderstorm wind    fatalites 1.220e+03
## 67           Tornado    fatalites 5.639e+03
## 68    Tropical Storm    fatalites 6.600e+01
```


### 4- Results
#### 4.1  The most harmful for people health top 5


```r
damgeOnHealth <- tidy[grep("fatalites|injures", tidy[, "damagetype"]), ]

ggplot(damgeOnHealth, aes(x = reorder(event, -value), y = value, fill = damagetype)) + 
    geom_bar(stat = "identity") + labs(title = "Top harmful weather event for populaiton health", 
    x = "Event", y = "Damage on populaiton health (number of people") + scale_fill_manual(values = c("#EED094", 
    "#C70000"))
```

```
## Error: 沒有這個函數 "ggplot"
```

 Rank| Event   |  People Headth Harm (injuries+fatalites)| 
 -|--------|-------------|
1|           Tornado     |   97078 | 
 2|Thunderstorm wind    |    12608 | 
 3|             Heat    |    12400 | 
 4|       Flash flood   |     10250| 
 5|      Cold   |     7415| 



#### 4.2  The most harmful for economic value top 5
 Rank| Event   |  People Headth Harm (injuries+fatalites)| 
 -|--------|-------------|
 1|           Tornado     |   97078 | 
 2|Thunderstorm wind    |    12608 | 
 3|             Heat    |    12400 | 
 4|       Flash flood   |     10250| 
 5|      Cold   |     7415| 




```r

damgeOnEconomic <- tidy[grep("corpdmgvalue|propdmgvalue", tidy[, "damagetype"]), 
    ]

ggplot(damgeOnEconomic, aes(x = reorder(event, -value), y = value, fill = damagetype)) + 
    geom_bar(stat = "identity") + labs(title = "Top harmful weather event for economic ", 
    x = "Event", y = "Damage on economic value (US dollors)") + scale_fill_manual(values = c("#1E87DE", 
    "#4D7A49"))
```

```
## Error: 沒有這個函數 "ggplot"
```

```r

install.packages("xtable")
```

```
## Installing package into '/Users/chencheinchun/Library/R/3.0/library'
## (as 'lib' is unspecified)
```

```
## 
## The downloaded binary packages are in
## 	/var/folders/sj/chl2dtc11pqdm66sl_q6x12m0000gn/T//RtmpFUyvUz/downloaded_packages
```

```r
library(xtable)
sortedTable <- xtable(sort(tidy))
```

```
## Error: undefined columns selected
```

```r
print(sortedTable, type = "html")
```

```
## <!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
## <!-- Sun May 25 13:36:48 2014 -->
## <TABLE border=1>
## <TR> <TH>  </TH> <TH> event </TH> <TH> pephealthdmg </TH> <TH> ecodmgvalue </TH>  </TR>
##   <TR> <TD align="right"> 1 </TD> <TD> Cold </TD> <TD align="right"> 7415.00 </TD> <TD align="right"> 19394859712.70 </TD> </TR>
##   <TR> <TD align="right"> 2 </TD> <TD> Drought </TD> <TD align="right"> 25.00 </TD> <TD align="right"> 15018927780.00 </TD> </TR>
##   <TR> <TD align="right"> 3 </TD> <TD> Dust devil </TD> <TD align="right"> 45.00 </TD> <TD align="right"> 719130.00 </TD> </TR>
##   <TR> <TD align="right"> 4 </TD> <TD> Dust storm </TD> <TD align="right"> 462.00 </TD> <TD align="right"> 9199000.00 </TD> </TR>
##   <TR> <TD align="right"> 5 </TD> <TD> Flash flood </TD> <TD align="right"> 10250.00 </TD> <TD align="right"> 171548197179.71 </TD> </TR>
##   <TR> <TD align="right"> 6 </TD> <TD> Hail </TD> <TD align="right"> 1512.00 </TD> <TD align="right"> 20734204439.70 </TD> </TR>
##   <TR> <TD align="right"> 7 </TD> <TD> Heat </TD> <TD align="right"> 12400.00 </TD> <TD align="right"> 924549250.00 </TD> </TR>
##   <TR> <TD align="right"> 8 </TD> <TD> Hurricane </TD> <TD align="right"> 1463.00 </TD> <TD align="right"> 90271472810.00 </TD> </TR>
##   <TR> <TD align="right"> 9 </TD> <TD> Ligntning </TD> <TD align="right"> 6049.00 </TD> <TD align="right"> 951105036.70 </TD> </TR>
##   <TR> <TD align="right"> 10 </TD> <TD> Other </TD> <TD align="right"> 4222.00 </TD> <TD align="right"> 10149749430.00 </TD> </TR>
##   <TR> <TD align="right"> 11 </TD> <TD> Rain </TD> <TD align="right"> 419.00 </TD> <TD align="right"> 4184512992.00 </TD> </TR>
##   <TR> <TD align="right"> 12 </TD> <TD> Rip current </TD> <TD align="right"> 1106.00 </TD> <TD align="right"> 163000.00 </TD> </TR>
##   <TR> <TD align="right"> 13 </TD> <TD> River flood </TD> <TD align="right"> 8.00 </TD> <TD align="right"> 10318413600.00 </TD> </TR>
##   <TR> <TD align="right"> 14 </TD> <TD> Sunami </TD> <TD align="right"> 162.00 </TD> <TD align="right"> 144082000.00 </TD> </TR>
##   <TR> <TD align="right"> 15 </TD> <TD> Thunderstorm wind </TD> <TD align="right"> 12608.00 </TD> <TD align="right"> 66943744393.20 </TD> </TR>
##   <TR> <TD align="right"> 16 </TD> <TD> Tornado </TD> <TD align="right"> 97078.00 </TD> <TD align="right"> 57417921848.70 </TD> </TR>
##   <TR> <TD align="right"> 17 </TD> <TD> Tropical Storm </TD> <TD align="right"> 449.00 </TD> <TD align="right"> 8411023550.00 </TD> </TR>
##    </TABLE>
```
=======
### 2 - Analysis Question
 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to popuulation health
 2.  Across the United States, which types of events have the greatest economic consequences
 
 
### 3- Data Processing


#### 3.1 Data cleanning


#### 3.2 Data extracting
  

  

### 3- Results



>>>>>>> FETCH_HEAD

