# Analysis on Various Weather Events aganist Population Health and Economic

## 1- Synopsis
  - This report presents an analysis about the impacts of wheather events on the population health and economic.We analysis the data published by the National Weather Service Instruction in 2007. We will address the damge on economic  by reporting the economic dmange caculated by property damge and crop damge measuered in US dollars, and address the damage on people health by sum the number of injures and fatalites in the events. 

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


- 2. Variables we keep for analysis

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

      - Group all events with five main category
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
    RAIN  | rain |



```r
# Function : Give each eventype an unified event description
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
            dd[indexFit, "event"] <- factorId[i]
            
        }
    }
    return(dd)
}
```


  - Use the function to filter and put value into "event"

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
##              4987              2512               150               429 
##       Flash flood              Hail              Heat         Hurricane 
##             25455            288672               768            481449 
##         Ligntning             Other              Rain            Sunami 
##             15776              4840             12238                20 
## Thunderstorm wind           Tornado 
##               438             64563
```



#### 3.1.2  Create an exact amounts for property and corp damage
        - Fomula I  : propdmgValue = propdmg       *  propdmgexp
        - Fomula II : cropdmgValue = cropdmg       *  cropdmgexp  

        
              propdmgexp   |  unit   
           ----------------| ----------------
              B            |  1,000,000,000
              M            |      1,000,000   
              K            |          1,000 
              H            |            100
              NA or BLANK  |              1


```r
# Function : Caculate each entry the property and crop damge value by 'dmg'
# and 'exp' variables
deliverActualDmgValue <- function(dd) {
    unit <- data.frame(cha = c("B", "M", "K", "H"), val = c(1e+09, 1e+06, 1000, 
        100))
    multi <- c(1e+09, 1e+06, 1000, 100)
    
    for (i in 1:nrow(unit)) {
        indexCrodFit <- grep(unit[i, "cha"], toupper(dd[, "cropdmgexp"]))
        if (length(indexCrodFit) > 0) {
            dd[indexCrodFit, "cropdmgvalue"] <- multi[i] * dd[indexCrodFit, 
                "cropdmg"]
        }
        indexProdFit <- grep(unit[i, "cha"], toupper(dd[, "propdmgexp"]))
        if (length(indexProdFit) > 0) {
            dd[indexProdFit, "propdmgvalue"] <- multi[i] * dd[indexProdFit, 
                "propdmg"]
        }
    }
    return(dd)
}


pData$cropdmgvalue <- pData$cropdmg
pData$propdmgvalue <- pData$propdmg
pData <- deliverActualDmgValue(pData)
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

```r


# Add a new variable: propdmagevalue + corpdmgvalue
pData$ecodmgvalue <- pData$cropdmgvalue + pData$propdmgvalue
pData$pephealthdmg <- pData$injures + pData$fatalites
summary(pData$ecodmgvalue)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.00e+00 0.00e+00 0.00e+00 5.28e+05 1.00e+03 1.15e+11
```

          
#### 3.2 Data Aggeratiion

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
## 1               Cold propdmgvalue 7.009e+08
## 2            Drought propdmgvalue 1.046e+09
## 3         Dust devil propdmgvalue 7.191e+05
## 4         Dust storm propdmgvalue 5.599e+06
## 5        Flash flood propdmgvalue 1.448e+11
## 6               Hail propdmgvalue 1.597e+10
## 7               Heat propdmgvalue 1.797e+06
## 8          Hurricane propdmgvalue 1.165e+11
## 9          Ligntning propdmgvalue 9.390e+08
## 10             Other propdmgvalue 8.699e+10
## 11              Rain propdmgvalue 3.265e+09
## 12            Sunami propdmgvalue 1.441e+08
## 13 Thunderstorm wind propdmgvalue 1.040e+07
## 14           Tornado propdmgvalue 5.700e+10
## 15              Cold cropdmgvalue 1.729e+09
## 16           Drought cropdmgvalue 1.397e+10
## 17        Dust devil cropdmgvalue 0.000e+00
## 18        Dust storm cropdmgvalue 3.600e+06
## 19       Flash flood cropdmgvalue 5.671e+09
## 20              Hail cropdmgvalue 3.026e+09
## 21              Heat cropdmgvalue 4.015e+08
## 22         Hurricane cropdmgvalue 1.728e+10
## 23         Ligntning cropdmgvalue 1.210e+07
## 24             Other cropdmgvalue 5.668e+09
## 25              Rain cropdmgvalue 9.193e+08
## 26            Sunami cropdmgvalue 2.000e+04
## 27 Thunderstorm wind cropdmgvalue 1.810e+06
## 28           Tornado cropdmgvalue 4.150e+08
## 29              Cold      injures 1.025e+03
## 30           Drought      injures 1.900e+01
## 31        Dust devil      injures 4.300e+01
## 32        Dust storm      injures 4.400e+02
## 33       Flash flood      injures 6.791e+03
## 34              Hail      injures 1.361e+03
## 35              Heat      injures 2.100e+03
## 36         Hurricane      injures 2.812e+04
## 37         Ligntning      injures 5.232e+03
## 38             Other      injures 3.417e+03
## 39              Rain      injures 3.050e+02
## 40            Sunami      injures 1.290e+02
## 41 Thunderstorm wind      injures 1.260e+02
## 42           Tornado      injures 9.142e+04
## 43              Cold    fatalites 1.540e+02
## 44           Drought    fatalites 6.000e+00
## 45        Dust devil    fatalites 2.000e+00
## 46        Dust storm    fatalites 2.200e+01
## 47       Flash flood    fatalites 4.760e+02
## 48              Hail    fatalites 1.500e+01
## 49              Heat    fatalites 9.370e+02
## 50         Hurricane    fatalites 6.353e+03
## 51         Ligntning    fatalites 8.170e+02
## 52             Other    fatalites 5.500e+02
## 53              Rain    fatalites 1.140e+02
## 54            Sunami    fatalites 3.300e+01
## 55 Thunderstorm wind    fatalites 2.700e+01
## 56           Tornado    fatalites 5.639e+03
```


### 4- Results
#### 4.1  The most harmful for people health
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


 
#### 4.2  The most harmful for economic value 





```r

damgeOnEconomic <- tidy[grep("cropdmgvalue|propdmgvalue", tidy[, "damagetype"]), 
    ]
ggplot(damgeOnEconomic, aes(x = reorder(event, value), y = value, fill = factor(damagetype, 
    labels = c("Property", "Crop")))) + geom_bar(stat = "identity") + labs(title = "Top harmful weather event for economic ", 
    x = "Event", y = "Damage on economic value (US dollors)") + scale_fill_manual(values = c("#7D5E35", 
    "#407D47")) + guides(fill = guide_legend(title = "Type of damage")) + xlab("Weather Event") + 
    theme(axis.text = element_text(size = 12, colour = "#1F4178"), axis.title = element_text(size = 14, 
        colour = "#3A3E42", face = "bold"), title = element_text(size = 16, 
        colour = "#282B2E", face = "bold")) + coord_flip()
```

![plot of chunk plot_ecodmg](figure/plot_ecodmg.png) 



