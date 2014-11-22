Practical Machine Learning - project
========================================================

Let's load training data ...

```r
library(caret)
pml.training <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
names(pml.training)
```

```
##   [1] "X"                        "user_name"               
##   [3] "raw_timestamp_part_1"     "raw_timestamp_part_2"    
##   [5] "cvtd_timestamp"           "new_window"              
##   [7] "num_window"               "roll_belt"               
##   [9] "pitch_belt"               "yaw_belt"                
##  [11] "total_accel_belt"         "kurtosis_roll_belt"      
##  [13] "kurtosis_picth_belt"      "kurtosis_yaw_belt"       
##  [15] "skewness_roll_belt"       "skewness_roll_belt.1"    
##  [17] "skewness_yaw_belt"        "max_roll_belt"           
##  [19] "max_picth_belt"           "max_yaw_belt"            
##  [21] "min_roll_belt"            "min_pitch_belt"          
##  [23] "min_yaw_belt"             "amplitude_roll_belt"     
##  [25] "amplitude_pitch_belt"     "amplitude_yaw_belt"      
##  [27] "var_total_accel_belt"     "avg_roll_belt"           
##  [29] "stddev_roll_belt"         "var_roll_belt"           
##  [31] "avg_pitch_belt"           "stddev_pitch_belt"       
##  [33] "var_pitch_belt"           "avg_yaw_belt"            
##  [35] "stddev_yaw_belt"          "var_yaw_belt"            
##  [37] "gyros_belt_x"             "gyros_belt_y"            
##  [39] "gyros_belt_z"             "accel_belt_x"            
##  [41] "accel_belt_y"             "accel_belt_z"            
##  [43] "magnet_belt_x"            "magnet_belt_y"           
##  [45] "magnet_belt_z"            "roll_arm"                
##  [47] "pitch_arm"                "yaw_arm"                 
##  [49] "total_accel_arm"          "var_accel_arm"           
##  [51] "avg_roll_arm"             "stddev_roll_arm"         
##  [53] "var_roll_arm"             "avg_pitch_arm"           
##  [55] "stddev_pitch_arm"         "var_pitch_arm"           
##  [57] "avg_yaw_arm"              "stddev_yaw_arm"          
##  [59] "var_yaw_arm"              "gyros_arm_x"             
##  [61] "gyros_arm_y"              "gyros_arm_z"             
##  [63] "accel_arm_x"              "accel_arm_y"             
##  [65] "accel_arm_z"              "magnet_arm_x"            
##  [67] "magnet_arm_y"             "magnet_arm_z"            
##  [69] "kurtosis_roll_arm"        "kurtosis_picth_arm"      
##  [71] "kurtosis_yaw_arm"         "skewness_roll_arm"       
##  [73] "skewness_pitch_arm"       "skewness_yaw_arm"        
##  [75] "max_roll_arm"             "max_picth_arm"           
##  [77] "max_yaw_arm"              "min_roll_arm"            
##  [79] "min_pitch_arm"            "min_yaw_arm"             
##  [81] "amplitude_roll_arm"       "amplitude_pitch_arm"     
##  [83] "amplitude_yaw_arm"        "roll_dumbbell"           
##  [85] "pitch_dumbbell"           "yaw_dumbbell"            
##  [87] "kurtosis_roll_dumbbell"   "kurtosis_picth_dumbbell" 
##  [89] "kurtosis_yaw_dumbbell"    "skewness_roll_dumbbell"  
##  [91] "skewness_pitch_dumbbell"  "skewness_yaw_dumbbell"   
##  [93] "max_roll_dumbbell"        "max_picth_dumbbell"      
##  [95] "max_yaw_dumbbell"         "min_roll_dumbbell"       
##  [97] "min_pitch_dumbbell"       "min_yaw_dumbbell"        
##  [99] "amplitude_roll_dumbbell"  "amplitude_pitch_dumbbell"
## [101] "amplitude_yaw_dumbbell"   "total_accel_dumbbell"    
## [103] "var_accel_dumbbell"       "avg_roll_dumbbell"       
## [105] "stddev_roll_dumbbell"     "var_roll_dumbbell"       
## [107] "avg_pitch_dumbbell"       "stddev_pitch_dumbbell"   
## [109] "var_pitch_dumbbell"       "avg_yaw_dumbbell"        
## [111] "stddev_yaw_dumbbell"      "var_yaw_dumbbell"        
## [113] "gyros_dumbbell_x"         "gyros_dumbbell_y"        
## [115] "gyros_dumbbell_z"         "accel_dumbbell_x"        
## [117] "accel_dumbbell_y"         "accel_dumbbell_z"        
## [119] "magnet_dumbbell_x"        "magnet_dumbbell_y"       
## [121] "magnet_dumbbell_z"        "roll_forearm"            
## [123] "pitch_forearm"            "yaw_forearm"             
## [125] "kurtosis_roll_forearm"    "kurtosis_picth_forearm"  
## [127] "kurtosis_yaw_forearm"     "skewness_roll_forearm"   
## [129] "skewness_pitch_forearm"   "skewness_yaw_forearm"    
## [131] "max_roll_forearm"         "max_picth_forearm"       
## [133] "max_yaw_forearm"          "min_roll_forearm"        
## [135] "min_pitch_forearm"        "min_yaw_forearm"         
## [137] "amplitude_roll_forearm"   "amplitude_pitch_forearm" 
## [139] "amplitude_yaw_forearm"    "total_accel_forearm"     
## [141] "var_accel_forearm"        "avg_roll_forearm"        
## [143] "stddev_roll_forearm"      "var_roll_forearm"        
## [145] "avg_pitch_forearm"        "stddev_pitch_forearm"    
## [147] "var_pitch_forearm"        "avg_yaw_forearm"         
## [149] "stddev_yaw_forearm"       "var_yaw_forearm"         
## [151] "gyros_forearm_x"          "gyros_forearm_y"         
## [153] "gyros_forearm_z"          "accel_forearm_x"         
## [155] "accel_forearm_y"          "accel_forearm_z"         
## [157] "magnet_forearm_x"         "magnet_forearm_y"        
## [159] "magnet_forearm_z"         "classe"
```

```r
table(complete.cases(pml.training))
```

```
## 
## FALSE  TRUE 
## 19216   406
```
The training data look sparse , so we will use only the numerical properties of which contain the complete data


```r
colClass <-lapply(pml.training, class)
colVect =c()
for(cc in colClass){colVect <- c(colVect,cc)}
colVect
```

```
##   [1] "integer" "factor"  "integer" "integer" "factor"  "factor"  "integer"
##   [8] "numeric" "numeric" "numeric" "integer" "factor"  "factor"  "factor" 
##  [15] "factor"  "factor"  "factor"  "numeric" "integer" "factor"  "numeric"
##  [22] "integer" "factor"  "numeric" "integer" "factor"  "numeric" "numeric"
##  [29] "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"
##  [36] "numeric" "numeric" "numeric" "numeric" "integer" "integer" "integer"
##  [43] "integer" "integer" "integer" "numeric" "numeric" "numeric" "integer"
##  [50] "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"
##  [57] "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "integer"
##  [64] "integer" "integer" "integer" "integer" "integer" "factor"  "factor" 
##  [71] "factor"  "factor"  "factor"  "factor"  "numeric" "numeric" "integer"
##  [78] "numeric" "numeric" "integer" "numeric" "numeric" "integer" "numeric"
##  [85] "numeric" "numeric" "factor"  "factor"  "factor"  "factor"  "factor" 
##  [92] "factor"  "numeric" "numeric" "factor"  "numeric" "numeric" "factor" 
##  [99] "numeric" "numeric" "factor"  "integer" "numeric" "numeric" "numeric"
## [106] "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"
## [113] "numeric" "numeric" "numeric" "integer" "integer" "integer" "integer"
## [120] "integer" "numeric" "numeric" "numeric" "numeric" "factor"  "factor" 
## [127] "factor"  "factor"  "factor"  "factor"  "numeric" "numeric" "factor" 
## [134] "numeric" "numeric" "factor"  "numeric" "numeric" "factor"  "integer"
## [141] "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"
## [148] "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "integer"
## [155] "integer" "integer" "integer" "numeric" "numeric" "factor"
```

```r
colVect <-c()
for(i in 1:length(colClass)){colVect<-c(colVect,grepl("numeric",colClass[i])&&(!anyNA(pml.training[,i])))}
colVect[length(colVect)] <- TRUE

train <- pml.training[,colVect]
head(train)
```

```
##   roll_belt pitch_belt yaw_belt gyros_belt_x gyros_belt_y gyros_belt_z
## 1      1.41       8.07    -94.4         0.00         0.00        -0.02
## 2      1.41       8.07    -94.4         0.02         0.00        -0.02
## 3      1.42       8.07    -94.4         0.00         0.00        -0.02
## 4      1.48       8.05    -94.4         0.02         0.00        -0.03
## 5      1.48       8.07    -94.4         0.02         0.02        -0.02
## 6      1.45       8.06    -94.4         0.02         0.00        -0.02
##   roll_arm pitch_arm yaw_arm gyros_arm_x gyros_arm_y gyros_arm_z
## 1     -128      22.5    -161        0.00        0.00       -0.02
## 2     -128      22.5    -161        0.02       -0.02       -0.02
## 3     -128      22.5    -161        0.02       -0.02       -0.02
## 4     -128      22.1    -161        0.02       -0.03        0.02
## 5     -128      22.1    -161        0.00       -0.03        0.00
## 6     -128      22.0    -161        0.02       -0.03        0.00
##   roll_dumbbell pitch_dumbbell yaw_dumbbell gyros_dumbbell_x
## 1         13.05         -70.49       -84.87                0
## 2         13.13         -70.64       -84.71                0
## 3         12.85         -70.28       -85.14                0
## 4         13.43         -70.39       -84.87                0
## 5         13.38         -70.43       -84.85                0
## 6         13.38         -70.82       -84.47                0
##   gyros_dumbbell_y gyros_dumbbell_z magnet_dumbbell_z roll_forearm
## 1            -0.02             0.00               -65         28.4
## 2            -0.02             0.00               -64         28.3
## 3            -0.02             0.00               -63         28.3
## 4            -0.02            -0.02               -60         28.1
## 5            -0.02             0.00               -68         28.0
## 6            -0.02             0.00               -66         27.9
##   pitch_forearm yaw_forearm gyros_forearm_x gyros_forearm_y
## 1         -63.9        -153            0.03            0.00
## 2         -63.9        -153            0.02            0.00
## 3         -63.9        -152            0.03           -0.02
## 4         -63.9        -152            0.02           -0.02
## 5         -63.9        -152            0.02            0.00
## 6         -63.9        -152            0.02           -0.02
##   gyros_forearm_z magnet_forearm_y magnet_forearm_z classe
## 1           -0.02              654              476      A
## 2           -0.02              661              473      A
## 3            0.00              658              469      A
## 4            0.00              658              469      A
## 5           -0.02              655              473      A
## 6           -0.03              660              478      A
```
build a model 

```r
fit.rf <- randomForest(train$classe ~. , data= train)
fit.rf
```

```
## 
## Call:
##  randomForest(formula = train$classe ~ ., data = train) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 0.36%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 5576    4    0    0    0   0.0007168
## B    9 3779    8    0    1   0.0047406
## C    0    9 3396   17    0   0.0075979
## D    0    0   14 3199    3   0.0052861
## E    0    1    1    4 3601   0.0016634
```
load and select test

```r
pml.testing <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
test <- pml.testing[,colVect]
head(test)
```

```
##   roll_belt pitch_belt yaw_belt gyros_belt_x gyros_belt_y gyros_belt_z
## 1    123.00      27.00    -4.75        -0.50        -0.02        -0.46
## 2      1.02       4.87   -88.90        -0.06        -0.02        -0.07
## 3      0.87       1.82   -88.50         0.05         0.02         0.03
## 4    125.00     -41.60   162.00         0.11         0.11        -0.16
## 5      1.35       3.33   -88.60         0.03         0.02         0.00
## 6     -5.92       1.59   -87.70         0.10         0.05        -0.13
##   roll_arm pitch_arm yaw_arm gyros_arm_x gyros_arm_y gyros_arm_z
## 1     40.7    -27.80     178       -1.65        0.48       -0.18
## 2      0.0      0.00       0       -1.17        0.85       -0.43
## 3      0.0      0.00       0        2.10       -1.36        1.13
## 4   -109.0     55.00    -142        0.22       -0.51        0.92
## 5     76.1      2.76     102       -1.96        0.79       -0.54
## 6      0.0      0.00       0        0.02        0.05       -0.07
##   roll_dumbbell pitch_dumbbell yaw_dumbbell gyros_dumbbell_x
## 1        -17.74          24.96       126.24             0.64
## 2         54.48         -53.70       -75.51             0.34
## 3         57.07         -51.37       -75.20             0.39
## 4         43.11         -30.05      -103.32             0.10
## 5       -101.38         -53.44       -14.20             0.29
## 6         62.19         -50.56       -71.12            -0.59
##   gyros_dumbbell_y gyros_dumbbell_z magnet_dumbbell_z roll_forearm
## 1             0.06            -0.61               -56          141
## 2             0.05            -0.71               -36          109
## 3             0.14            -0.34                41          131
## 4            -0.02             0.05                53            0
## 5            -0.47            -0.46               312         -176
## 6             0.80             1.10                96          150
##   pitch_forearm yaw_forearm gyros_forearm_x gyros_forearm_y
## 1         49.30       156.0            0.74           -3.34
## 2        -17.60       106.0            1.12           -2.78
## 3        -32.60        93.0            0.18           -0.79
## 4          0.00         0.0            1.38            0.69
## 5         -2.16       -47.9           -0.75            3.10
## 6          1.46        89.7           -0.88            4.26
##   gyros_forearm_z magnet_forearm_y magnet_forearm_z problem_id
## 1           -0.59              419              617          1
## 2           -0.18              791              873          2
## 3            0.28              698              783          3
## 4            1.80              783              521          4
## 5            0.80             -787               91          5
## 6            1.35              800              884          6
```
predict test

```r
rf.pred <- predict(fit.rf, test)
rf.pred
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```





