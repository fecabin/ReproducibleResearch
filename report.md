# Analysis on Various Weather Events aganist Population Health and Economy

## 1- Synopsis
This report presents an analysis about the impacts of wheather events on the population health and economic. We analysed the data published by the National Weather Service Instruction in 2007. We will address the damge on economic  by reporting the economic dmange caculated by property damge and crop damge measuered in US dollars, and address the damage on people health by sum the number of injures and fatalites in the events. In the end, we illustrate the impact of different types of events on these two aspects with two barchart containing two measurements for each aspect. We found that the tornado is most harmful for population heath, and the flash flood results in greatest loss on economic value.

## 2 - Analysis Question
 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to popuulation health
 2.  Across the United States, which types of events have the greatest economic consequences
 
## 3- Data Processing

- 1. Load data

```r

library(ggplot2)  # plot 

```


```r
# attributes

ff <- file.path(getwd(), "repdata-data-StormData.csv.bz2")
data <- read.csv(ff, stringsAsFactors = FALSE)
```


- 2- Variables we keep for analysis

  Variabl Name | Description|
  -------------|-------------|
   state      | Abbervation for states in US |
   evtype     | Event type |
   fatalites  | The number of people died|
   injures   | The number of people injuured|
   propdmg    | The amount of property damge (measured in money)|
   propdmgexp  | The unit of  damge (B,M,K,H)|
   cropdmg  | The amount of corp damge (measured in money)|
   cropdmgexp    | The unit of  damge (B,M,K,H)|



```r
# attributes

attributes <- c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
    "CROPDMG", "CROPDMGEXP")

pData <- data[, attributes]
colnames(pData) <- c("state", "evtype", "fatalites", "injures", "propdmg", "propdmgexp", 
    "cropdmg", "cropdmgexp")
names(pData)
```

```
## [1] "state"      "evtype"     "fatalites"  "injures"    "propdmg"   
## [6] "propdmgexp" "cropdmg"    "cropdmgexp"
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
          - runami
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
          
#### The unified event name and filtr expression:
  Event    |    Filter expression  |  
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
  Rain  | RAIN |



```r
# Function : Give each eventype an unified event description
deliverEvtUnifiedName <- function(dd) {
    evtMatcher <- data.frame(reg = c("NADO|FUNNEL|WATERSPOUT", "THUNDER|STORM|WIND", 
        "HAIL", "FROST|FREEZ|BLIZZARD|WINTER|COLD|LOW TEMP|RECORD LOW|SNOW|ICE", 
        "HEAT|WARM|RECORD HIGH", "COSTAL STORM", "SUNAMI", "RIP CURRENT", "FLASH FLOOD|FLD|FLOOD", 
        "RIVER FLOOD|URBAN FLOOD", "TROPICAL STORM|TROPICAL", "HURRICANE", "DROUGHT", 
        "DUST STORM", "DUST DEVIL", "RAIN", "LIGHTNING"))
    
    factorId <- c("Tornado", "Thunderstorm wind", "Hail", "Cold", "Heat", "Costal Storm", 
        "Sunami", "Rip current", "Flash flood", "River flood", "Tropical Storm", 
        "Hurricane", "Drought", "Dust storm", "Dust devil", "Rain", "Ligntning")
    
    for (i in 1:nrow(evtMatcher)) {
        indexFit <- grep(evtMatcher[i, "reg"], toupper(dd[, "evtype"]))
        if (length(indexFit) > 0) {
            dd[indexFit, "event"] <- factorId[i]
            
        }
    }
    return(dd)
}
```


#### Use the function to filter and put value into "event"

```r


pData$event <- ("-")

# Give each event an unified id factor (17+1 factors)
pData <- deliverEvtUnifiedName(pData)
otherIndex <- grep("-", pData[, "event"])
pData[otherIndex, "event"] <- "Other"
pData$event <- as.factor(pData$event)

# See the event and number of that
table(pData$event)
```

```
## 
##              Cold           Drought        Dust devil        Dust storm 
##             46131              2512               150               429 
##       Flash flood              Hail              Heat         Hurricane 
##             85530            290400              2969               288 
##         Ligntning             Other              Rain       Rip current 
##             15776              9558             12238               777 
##       River flood            Sunami Thunderstorm wind           Tornado 
##               569                20            362662             71531 
##    Tropical Storm 
##               757
```



### 3.1.2  Create an exact amounts for property and corp damage

  - Fomula I  : propdmgValue = propdmg       *  propdmgexp
  - Fomula II : cropdmgValue = cropdmg       *  cropdmgexp  

        
              propdmgexp   |  unit   
           ----------------| ----------------
              B            |  1,000,000,000
              M            |      1,000,000   
              K            |          1,000 
              H            |            100
              NA or BLANK  |              1

#### Function : Caculate the property and crop damg by 'dmg' and 'exp' variable
  

```r
# Function : Caculate each entry the property and crop damge value by 'dmg'
# and 'exp' variables
deliverActualDmgValue <- function(dd) {
    unit <- data.frame(cha = c("B", "M", "K", "H"), val = c(1e+09, 1e+06, 1000, 
        100))
    multi <- c(1e+09, 1e+06, 1000, 100)
    
    for (i in 1:nrow(unit)) {
        # index that match the unit
        indexCrodFit <- grep(unit[i, "cha"], toupper(dd[, "cropdmgexp"]))
        
        if (length(indexCrodFit) > 0) {
            
            # Caculate the actual value
            dd[indexCrodFit, "cropdmgvalue"] <- multi[i] * dd[indexCrodFit, 
                "cropdmg"]
            
        }
        
        # Same procudure for property damage
        indexProdFit <- grep(unit[i, "cha"], toupper(dd[, "propdmgexp"]))
        
        if (length(indexProdFit) > 0) {
            
            dd[indexProdFit, "propdmgvalue"] <- multi[i] * dd[indexProdFit, 
                "propdmg"]
        }
        
    }
    return(dd)
}

# Default value of the damage equals to variable 'dmg'
pData$cropdmgvalue <- pData$cropdmg
pData$propdmgvalue <- pData$propdmg

pData <- deliverActualDmgValue(pData)
```

#### Watch the summary of crop damage and property damage

```r

# see the value:
summary(pData$cropdmgvalue)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.00e+00 0.00e+00 0.00e+00 5.44e+04 0.00e+00 5.00e+09
```

```r
summary(pData$propdmgvalue)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.00e+00 0.00e+00 0.00e+00 4.74e+05 5.00e+02 1.15e+11
```


### Add  new variables: Damge on economic, Damge on population

 - Damge on economic : `ecodmgvalue` = `propdmagevalue` + `corpdmgvalue`
 - Damge on population health : `pephealthdmg` = `propdmagevalue` + `corpdmgvalue`
    

```r
pData$ecodmgvalue <- pData$cropdmgvalue + pData$propdmgvalue
pData$pephealthdmg <- pData$injures + pData$fatalites
summary(pData$ecodmgvalue)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.00e+00 0.00e+00 0.00e+00 5.28e+05 1.00e+03 1.15e+11
```

          
### 3.2 Data Aggeratiion

Caculate the Sum of `propdmgvalue`, `cropdmgvalue`,`injures`,`fatalites` for each `event` 


```r
library(reshape2)

t <- aggregate(cbind(propdmgvalue, cropdmgvalue, injures, fatalites) ~ event, 
    pData, FUN = sum)
tidy <- melt(t, id.var = "event", variable.name = "variable")
colnames(tidy) <- c("event", "damagetype", "value")
print(tidy)
```

```
##                event   damagetype     value
## 1               Cold propdmgvalue 1.268e+10
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
## 18              Cold cropdmgvalue 8.730e+09
## 19           Drought cropdmgvalue 1.397e+10
## 20        Dust devil cropdmgvalue 0.000e+00
## 21        Dust storm cropdmgvalue 3.600e+06
## 22       Flash flood cropdmgvalue 7.217e+09
## 23              Hail cropdmgvalue 3.114e+09
## 24              Heat cropdmgvalue 9.044e+08
## 25         Hurricane cropdmgvalue 5.515e+09
## 26         Ligntning cropdmgvalue 1.210e+07
## 27             Other cropdmgvalue 5.722e+08
## 28              Rain cropdmgvalue 9.193e+08
## 29       Rip current cropdmgvalue 0.000e+00
## 30       River flood cropdmgvalue 5.059e+09
## 31            Sunami cropdmgvalue 2.000e+04
## 32 Thunderstorm wind cropdmgvalue 1.975e+09
## 33           Tornado cropdmgvalue 4.150e+08
## 34    Tropical Storm cropdmgvalue 6.949e+08
## 35              Cold      injures 6.350e+03
## 36           Drought      injures 1.900e+01
## 37        Dust devil      injures 4.300e+01
## 38        Dust storm      injures 4.400e+02
## 39       Flash flood      injures 8.680e+03
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
## 52              Cold    fatalites 1.088e+03
## 53           Drought    fatalites 6.000e+00
## 54        Dust devil    fatalites 2.000e+00
## 55        Dust storm    fatalites 2.200e+01
## 56       Flash flood    fatalites 1.547e+03
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


## 4- Results
### 4.1  The most harmful weather events for people health
  - Extract rows that contains info about people impact on populaiton health

```r
library("ggplot2")
damgeOnHealth <- tidy[grep("fatalites|injures", tidy[, "damagetype"]), ]

ggplot(damgeOnHealth, aes(x = reorder(event, value), y = value, fill = factor(damagetype, 
    labels = c("Injures", "Fatalites")))) + geom_bar(stat = "identity") + labs(title = "Top harmful weather event for populaiton health", 
    x = "Event", y = "Damage on populaiton health (number of people)") + scale_fill_manual(values = c("#F7B388", 
    "#C70000")) + guides(fill = guide_legend(title = "Type of damage")) + xlab("Weather Event") + 
    theme(axis.text = element_text(size = 12, colour = "#1F4178"), axis.title = element_text(size = 14, 
        colour = "#3A3E42", face = "bold"), title = element_text(size = 16, 
        colour = "#282B2E", face = "bold")) + coord_flip()
```

![plot of chunk plot_healthdmg](figure/plot_healthdmg.png) 


 
### 4.2  The most harmful weather events for economic value 

 - Extract rows that contains info about people impact on economy 
 

```r
damgeOnEconomic <- tidy[grep("cropdmgvalue|propdmgvalue", tidy[, "damagetype"]), 
    ]
ggplot(damgeOnEconomic, aes(x = reorder(event, value), y = value, fill = factor(damagetype, 
    labels = c("Property", "Crop")))) + geom_bar(stat = "identity") + labs(title = "Top harmful weather event for economy ", 
    x = "Event", y = "Damage on economic value (US dollors)") + scale_fill_manual(values = c("#7D5E35", 
    "#407D47")) + guides(fill = guide_legend(title = "Type of damage")) + xlab("Weather Event") + 
    theme(axis.text = element_text(size = 12, colour = "#1F4178"), axis.title = element_text(size = 14, 
        colour = "#3A3E42", face = "bold"), title = element_text(size = 16, 
        colour = "#282B2E", face = "bold")) + coord_flip()
```

![plot of chunk plot_ecodmg](figure/plot_ecodmg.png) 


## 5- Conclusion
  From the results, we can see :
  - 1. The most harmful weather event for population health is ***tornado*** 
  - 2. The most harmful weather event for economy is ***flash flood***




