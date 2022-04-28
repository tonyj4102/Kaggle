###fix date time , and N/A columns
## sort features
### add more features less than^-16

### lesscorrelated features @ ^-16  - - - -  in progress

### double check list of correlated features (slides)
### logit reg parameters
### set decision boundary
### ridge/lasso with #=10/12/.etc.
### CART
### no N/A's
### feature scale
# (repeat step process with sig vars)
# R on Spark, Azure Machine Learning, 
#step selection, forward select, cross validation, ridge/etc. without two N/A columns
#step function
### Neural network, RandomForest

setwd("C:/Users/RCloud/Downloads")
train=read.csv("train-3.csv")
test=read.csv("test-4.csv")

###features by significance
GeographicField34B   7.206e-03  3.801e-03   1.896 0.058001 . 
GeographicField22A  -3.765e-03  1.998e-03  -1.884 0.059527 .
GeographicField55B  -6.520e-03  3.451e-03  -1.889 0.058872 . 
PropertyField7D   9.515e-02  5.165e-02   1.842 0.065433 . 
PersonalField51    1.050e+00  5.840e-01   1.798 0.072211 . 
PersonalField77   -2.068e-01  1.151e-01  -1.796 0.072455 .
GeographicField36A  -7.617e-03  4.394e-03  -1.733 0.083037 .
GeographicField64IL -3.818e-01  2.236e-01  -1.708 0.087698 . 
GeographicField1B    3.959e-03  2.270e-03   1.744 0.081194 .
GeographicField61A  -2.610e-03  1.511e-03  -1.728 0.084017 .
GeographicField37A   4.183e-03  2.435e-03   1.718 0.085867 . 
GeographicField3A   -1.369e-02  8.053e-03  -1.700 0.089075 .
GeographicField11B  -1.531e-02  9.015e-03  -1.698 0.089483 .
PersonalField53   -1.189e+00  4.659e-01  -2.551 0.010731 *
  GeographicField63N   1.120e-01  4.431e-02   2.527 0.011505 * 
  PropertyField11A -3.984e-03  1.653e-03  -2.410 0.015940 *
  GeographicField24B   1.356e-02  5.604e-03   2.419 0.015576 *
  GeographicField17B  -7.859e-03  3.342e-03  -2.352 0.018692 *
  PersonalField49    1.310e+00  5.600e-01   2.339 0.019348 *
  GeographicField32A  -8.817e-03  3.722e-03  -2.369 0.017842 *  
  GeographicField5A    5.860e-03  2.526e-03   2.320 0.020344 *  
  GeographicField5B   -1.355e-02  6.069e-03  -2.232 0.025603 * 
  GeographicField14B  -2.279e-02  9.836e-03  -2.317 0.020521 *
  PropertyField5Y   1.257e+00  5.952e-01   2.111 0.034753 *
  GeographicField24A  -1.865e-02  8.643e-03  -2.158 0.030955 *
  GeographicField60A   3.224e-03  1.535e-03   2.100 0.035737 * 
  GeographicField9B   -1.613e-02  8.056e-03  -2.002 0.045296 *
  GeographicField2A    1.936e-02  9.747e-03   1.986 0.046979 * 
  GeographicField32B   5.512e-03  2.740e-03   2.012 0.044240 * 
  PersonalField54    9.580e-01  2.970e-01   3.225 0.001258 **
  PersonalField4B   -1.626e-02  5.082e-03  -3.199 0.001379 **
  GeographicField20A  -2.188e-02  6.768e-03  -3.232 0.001227 **
  GeographicField7B    4.939e-02  1.568e-02   3.150 0.001633 ** 
  GeographicField36B   9.261e-03  2.994e-03   3.093 0.001982 ** 
  GeographicField19A   3.070e-02  1.023e-02   3.000 0.002701 **
  GeographicField8A    8.846e-02  2.978e-02   2.971 0.002971 ** 
  PropertyField14B -9.740e-02  3.211e-02  -3.034 0.002416 ** 
  PropertyField28D -0.162512   0.053823  -3.019  0.00253 ** 
  PersonalField63   -5.887e-01  1.980e-01  -2.973 0.002949 **
  PersonalField4A    1.723e-02  5.873e-03   2.933 0.003358 ** 
  GeographicField46A  -1.489e-02  5.053e-03  -2.947 0.003213 ** 
  PropertyField39A  -0.015409   0.005634  -2.735 0.006238 ** 
  GeographicField44A   1.819e-02  6.659e-03   2.732 0.006293 ** 
  GeographicField16A  -3.583e-02  1.307e-02  -2.742 0.006113 **
  PersonalField56    7.992e-01  3.000e-01   2.664 0.007715 **
  PersonalField33    9.358e-01  3.489e-01   2.682 0.007310 ** 
  PropertyField10  -2.816e-01  7.277e-02  -3.869 0.000109 ***
  GeographicField38A  -1.631e-02  4.314e-03  -3.781 0.000156 ***
  PropertyField31N   0.117721   0.031283   3.763 0.000168 ***
  PersonalField78    5.060e-01  1.307e-01   3.872 0.000108 ***
  GeographicField48B  -3.330e-02  8.928e-03  -3.730 0.000192 ***
  GeographicField8B   -4.894e-02  1.325e-02  -3.694 0.000221 ***
  GeographicField44B  -1.964e-02  5.380e-03  -3.651 0.000261 ***
  PersonalField41    2.893e+00  8.002e-01   3.616 0.000300 ***
  GeographicField23A  -1.218e-02  3.383e-03  -3.599 0.000320 ***
  GeographicField50A  -2.084e-02  5.772e-03  -3.610 0.000306 ***
  GeographicField46B   1.310e-02  3.660e-03   3.578 0.000346 ***
  GeographicField64NJ  5.662e-01  1.586e-01   3.570 0.000357 ***
  GeographicField17A  -8.014e-03  2.340e-03  -3.425 0.000616 ***
  PropertyField31O   0.088188   0.025824   3.415 0.000638 ***
  GeographicField13B   3.697e-02  1.087e-02   3.399 0.000675 ***
  PersonalField39    2.657e+00  7.841e-01   3.389 0.000702 ***
  PropertyField7H   2.794e-01  8.345e-02   3.348 0.000814 ***
  GeographicField1A   -1.046e-02  3.123e-03  -3.349 0.000810 ***
  CoverageField9C   -0.422006   0.127474  -3.311 0.000931 ***
  ##HOLD
  
  GeographicField38B   1.265e-02  3.138e-03   4.030 5.58e-05 ***
  PropertyField33H  -0.117184   0.028043  -4.179 2.93e-05 ***
  PersonalField48    1.125e+00  2.607e-01   4.314 1.60e-05 ***
  PersonalField29   -1.166e+00  2.642e-01  -4.411 1.03e-05 ***
  GeographicField6B    3.128e-02  7.624e-03   4.103 4.07e-05 ***
  GeographicField7A    9.578e-02  2.215e-02   4.325 1.53e-05 ***
  GeographicField12B  -6.314e-02  1.449e-02  -4.358 1.31e-05 ***
  GeographicField20B   1.457e-02  3.563e-03   4.088 4.35e-05 ***
  GeographicField50B   1.911e-02  4.901e-03   3.899 9.64e-05 ***
  GeographicField21B  -1.493e-02  3.565e-03  -4.189 2.80e-05 ***
  GeographicField23B   1.872e-02  4.152e-03   4.509 6.52e-06 ***
  GeographicField47A  -1.749e-02  3.679e-03  -4.754 1.99e-06 ***
  Day13              0.210806   0.047659   4.423 9.72e-06 ***
  SalesField11 -0.122110   0.026442   -4.618 3.87e-06 ***
  SalesField12  0.110561   0.024818    4.455 8.39e-06 ***
  PersonalField43   -3.247e+00  7.181e-01  -4.521 6.16e-06 ***
  GeographicField13A  -1.467e-01  3.279e-02  -4.473 7.71e-06 ***
  GeographicField26A   4.452e-02  9.210e-03   4.834 1.34e-06 ***
  CoverageField3A    0.028412   0.005422   5.240 1.60e-07 ***
  CoverageField2B   -0.034797   0.007018  -4.959 7.10e-07 ***
  PropertyField7F  -1.350e+00  2.552e-01  -5.292 1.21e-07 ***
  GeographicField25A  -8.281e-02  1.671e-02  -4.956 7.18e-07 ***
  GeographicField15B  -2.951e-02  5.510e-03  -5.355 8.53e-08 ***
  CoverageField11A  -0.019956   0.003684  -5.417 6.07e-08 ***
  PropertyField1A  -2.447e-02  4.476e-03  -5.467 4.59e-08 ***
  PersonalField25   -7.794e-01  1.419e-01  -5.495 3.92e-08 ***
  PersonalField22    6.533e-01  1.225e-01   5.331 9.75e-08 ***
  GeographicField47B   1.615e-02  2.989e-03   5.403 6.56e-08 ***
  GeographicField64TX -1.258e+00  2.198e-01  -5.726 1.03e-08 ***
  PropertyField22  -0.559277   0.097445  -5.739 9.50e-09 ***
  PersonalField58   -1.362e+00  2.245e-01  -6.067 1.31e-09 ***
  GeographicField27A   7.415e-02  1.219e-02   6.084 1.18e-09 ***
  PersonalField34   -2.409e+00  3.840e-01  -6.275 3.49e-10 ***
  CoverageField5B   -0.013613   0.002160  -6.301 2.95e-10 ***
  PropertyField12  -3.614e-02  5.881e-03  -6.146 7.95e-10 ***
  PropertyField16A -1.572e-02  2.540e-03  -6.188 6.10e-10 ***
  PersonalField83    7.331e-01  1.182e-01   6.200 5.66e-10 ***
  CoverageField8V    1.384289   0.205759   6.728 1.72e-11 ***
  PropertyField33F  -0.369260   0.056543  -6.531 6.55e-11 ***
  PropertyField18   6.684e-02  9.136e-03   7.316 2.55e-13 ***
  PersonalField6    -1.909e-01  2.737e-02  -6.976 3.05e-12 ***
  Field11           -4.011020   0.529324  -7.578 3.52e-14
CoverageField4A   -0.064990   0.008503  -7.643 2.13e-14 ***
  PropertyField21B  0.023129   0.003042   7.604 2.86e-14 ***
  PersonalField14   -3.982e-02  5.339e-03  -7.459 8.74e-14 ***
  PropertyField23  -0.020756   0.002597  -7.994 1.31e-15 ***
  Month12            0.425096   0.036900  11.520  < 2e-16 ***
  CoverageField3B    0.054843   0.004103  13.368  < 2e-16 ***
  CoverageField5A    0.022738   0.002153  10.559  < 2e-16 ***
  Field7            -0.037308   0.001074 -34.739  < 2e-16
CoverageField6A    0.046965   0.002201  21.339  < 2e-16 ***
  CoverageField6B   -0.053735   0.002059 -26.102  < 2e-16 ***
  CoverageField8U    1.282756   0.154197   8.319  < 2e-16 ***
  CoverageField8X    1.346866   0.081892  16.447  < 2e-16 ***
  CoverageField8Y    0.770997   0.039721  19.410  < 2e-16 ***
  CoverageField8Z    1.305888   0.111690  11.692  < 2e-16 ***
  CoverageField11B   0.032577   0.003608   9.029  < 2e-16 ***
  SalesField2A -0.129302   0.003999  -32.330  < 2e-16 ***
  SalesField3   0.427661   0.016766   25.508  < 2e-16 ***
  SalesField4   0.229534   0.008147   28.173  < 2e-16 ***
  SalesField5  -1.066237   0.010240 -104.128  < 2e-16 ***
  SalesField6  -0.023855   0.001227  -19.447  < 2e-16 ***
  SalesField9  -0.329148   0.020637  -15.949  < 2e-16 ***
  SalesField10  0.318192   0.013113   24.265  < 2e-16 ***
  SalesField13 -0.253285   0.024687  -10.260  < 2e-16 ***
  PropertyField1B   5.496e-02  3.933e-03  13.975  < 2e-16 ***
  PropertyField2A  -2.236e-02  1.759e-03 -12.711  < 2e-16 ***
  PropertyField2B  -9.530e-03  9.018e-04 -10.568  < 2e-16 ***
  PropertyField8    8.012e-01  1.491e-02  53.751  < 2e-16 ***
  PropertyField13  -2.000e-01  1.324e-02 -15.114  < 2e-16 ***
  PropertyField15   1.143e-01  3.424e-03  33.392  < 2e-16 ***
  PropertyField16B  3.304e-02  2.003e-03  16.499  < 2e-16 ***
  PropertyField17  -9.223e-02  1.146e-02  -8.051 8.18e-16 ***
  PropertyField19   0.144494   0.011982  12.059  < 2e-16 ***
  PropertyField21A -0.066402   0.004218 -15.742  < 2e-16 ***
  PropertyField24A  0.094948   0.005111  18.577  < 2e-16 ***
  PropertyField24B  0.059982   0.004129  14.527  < 2e-16 ***
  PropertyField25  -1.703014   0.034578 -49.252  < 2e-16 ***
  PropertyField26A -0.097607   0.004730 -20.634  < 2e-16 ***
  PropertyField26B -0.076644   0.003573 -21.451  < 2e-16 ***
  PropertyField28C -1.108281   0.128599  -8.618  < 2e-16 ***
  PropertyField30Y  -0.678280   0.032774 -20.696  < 2e-16 ***
  PropertyField33G  -0.756764   0.028247 -26.791  < 2e-16 ***
  PropertyField34N  -0.932151   0.014244 -65.441  < 2e-16 ***
  PropertyField34N  -0.932151   0.014244 -65.441  < 2e-16 ***
  PropertyField35    0.404066   0.012893  31.340  < 2e-16 ***
  PropertyField39B   0.037709   0.004382   8.605  < 2e-16 ***
  PersonalField28   -9.015e-01  9.041e-02  -9.972  < 2e-16 ***
  PersonalField19ZN  1.339e+00  1.479e-01   9.056  < 2e-16 *** //partial
PersonalField18YF  2.111e+00  2.211e-01   9.550  < 2e-16 ***  //partial
PersonalField17ZQ  1.508e+00  1.627e-01   9.270  < 2e-16 ***  //partial
PersonalField16ZW  9.854e-01  9.861e-02   9.993  < 2e-16 ***  //partial
PersonalField9    -1.932e+00  2.439e-02 -79.215  < 2e-16 ***
  PersonalField10A   4.416e-02  2.905e-03  15.198  < 2e-16 ***
  PersonalField10B  -1.334e-01  2.817e-03 -47.349  < 2e-16 ***
  PersonalField11    3.636e-01  3.622e-02  10.039  < 2e-16 ***
  PersonalField12   -8.471e-01  1.032e-02 -82.083  < 2e-16 ***
  PersonalField13   -4.484e+00  1.591e-01 -28.189  < 2e-16 ***
  PersonalField1     5.397e+00  8.858e-02  60.928  < 2e-16 ***
  PersonalField2    -4.986e+00  8.773e-02 -56.836  < 2e-16 ***

  
  #binary
  PropertyField38
PropertyField37
PropertyField36
PropertyField34
PropertyField32
PropertyField30
PropertyField5
PropertyField4
PropertyField3
PersonalField7

#categorical
GeographicField64
GeographicField63
PropertyField31
PropertyField28
PersonalField19
PersonalField18
PersonalField17
PersonalField16
CoverageField9
CoverageField8


#converting date filed to year, month, week and weekday fields   ============  Test & Train

train$Original_Quote_Date <- as.Date(train$Original_Quote_Date)
train$Day <- format(train$Original_Quote_Date, "%d")
train$Month <- format(train$Original_Quote_Date, "%m")
train$Year <- format(train$Original_Quote_Date, "%Y")
train$WeekdayDay <- weekdays(train$Original_Quote_Date)

test$Original_Quote_Date <- as.Date(test$Original_Quote_Date)
test$Day <- format(test$Original_Quote_Date, "%d")
test$Month <- format(test$Original_Quote_Date, "%m")
test$Year <- format(test$Original_Quote_Date, "%Y")
test$WeekdayDay <- weekdays(test$Original_Quote_Date)

test$Year=as.numeric(test$Year)
test$Month=as.numeric(test$Month)
test$Day=as.numeric(test$Day)

train$Year=as.numeric(train$Year)
train$Month=as.numeric(train$Month)
train$Day=as.numeric(train$Day)

#####split data

install.packages("caTools")
library(caTools)

set.seed(88)            	 ## So that we get the same set
split = sample.split(train$QuoteConversion_Flag, SplitRatio = 0.75)  
homeTrain = subset(train, split == TRUE)
homeTest = subset(train, split == FALSE)

Traincorr=homeTrain
Testcorr=homeTest

#separating numeric and character columns   =====  Test & Train
#Traincorr_num <- Traincorr[,names(Traincorr)[which(sapply(Traincorr, is.numeric))]]
#Traincorr_char <- Traincorr[,names(Traincorr)[which(sapply(Traincorr, is.character))]]
#Traincorr_fact <- Traincorr[,names(Traincorr)[which(sapply(Traincorr, is.factor))]]

cor(Traincorr_num)
write.csv(Traincorr_num,"Traincorr_num.csv") 


#convert character to numerical =====  Test & Train
Year
Month
Day

Traincorr$Year=as.numeric(Traincorr$Year)
Traincorr$Month=as.numeric(Traincorr$Month)
Traincorr$Day=as.numeric(Traincorr$Day)


Testcorr$Year=as.numeric(Testcorr$Year)
Testcorr$Month=as.numeric(Testcorr$Month)
Testcorr$Day=as.numeric(Testcorr$Day)

#convert factor to numeric

Traincorr$Field10 <- gsub(",","",Traincorr$Field10) #substitute "," with _ space  =====  Test & Train
Traincorr$Field10=as.numeric(Traincorr$Field10)
str(Traincorr$Field10)
head(Traincorr$Field10)

Testcorr=homeTest

Testcorr$Field10 <- gsub(",","",Testcorr$Field10) #substitute "," with _ space  =====  Test & Train
Testcorr$Field10=as.numeric(Testcorr$Field10)
str(Testcorr$Field10)
head(Testcorr$Field10)

####### MODEL16 ######   >>> LOGIT

###

#1
PersonalField19ZN  1.339e+00  1.479e-01   9.056  < 2e-16 *** //partial
PersonalField18YF  2.111e+00  2.211e-01   9.550  < 2e-16 ***  //partial
PersonalField17ZQ  1.508e+00  1.627e-01   9.270  < 2e-16 ***  //partial
PersonalField16ZW  9.854e-01  9.861e-02   9.993  < 2e-16 ***  //partial

PersonalField28   -9.015e-01  9.041e-02  -9.972  < 2e-16 ***
  PersonalField11    3.636e-01  3.622e-02  10.039  < 2e-16 ***
  PersonalField10A   4.416e-02  2.905e-03  15.198  < 2e-16 ***
  PersonalField13   -4.484e+00  1.591e-01 -28.189  < 2e-16 ***
  PersonalField10B  -1.334e-01  2.817e-03 -47.349  < 2e-16 ***
  PersonalField2    -4.986e+00  8.773e-02 -56.836  < 2e-16 ***
  PersonalField1     5.397e+00  8.858e-02  60.928  < 2e-16 ***
  PersonalField9    -1.932e+00  2.439e-02 -79.215  < 2e-16 ***
  PersonalField12   -8.471e-01  1.032e-02 -82.083  < 2e-16 ***
  
  #2
  Month12            0.425096   0.036900  11.520  < 2e-16 ***
  CoverageField3B    0.054843   0.004103  13.368  < 2e-16 ***
  CoverageField5A    0.022738   0.002153  10.559  < 2e-16 ***
  Field7            -0.037308   0.001074 -34.739  < 2e-16
CoverageField6A    0.046965   0.002201  21.339  < 2e-16 ***
  CoverageField6B   -0.053735   0.002059 -26.102  < 2e-16 ***
  CoverageField8U    1.282756   0.154197   8.319  < 2e-16 ***
  CoverageField8X    1.346866   0.081892  16.447  < 2e-16 ***
  CoverageField8Y    0.770997   0.039721  19.410  < 2e-16 ***
  CoverageField8Z    1.305888   0.111690  11.692  < 2e-16 ***
  CoverageField11B   0.032577   0.003608   9.029  < 2e-16 ***
  
#3  
  Month12+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8U+CoverageField8X+CoverageField8Y+CoverageField8Z+CoverageField11B 

#4
SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28C+PropertyField30Y+PropertyField33G+PropertyField34N+PropertyField34N+PropertyField35+PropertyField39B
  
#1-Interactions
PersonalField12:PersonalField9+PersonalField12:PersonalField1+PersonalField12:PersonalField2+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField12:PersonalField10A+PersonalField12:PersonalField11+PersonalField12:PersonalField28+PersonalField9:PersonalField1+PersonalField9:PersonalField2+PersonalField9:PersonalField10B+PersonalField9:PersonalField13+PersonalField9:PersonalField10A+PersonalField9:PersonalField11+PersonalField9:PersonalField28+PersonalField1:PersonalField2+PersonalField1:PersonalField10B+PersonalField1:PersonalField13+PersonalField1:PersonalField10A+PersonalField1:PersonalField11+PersonalField1:PersonalField28+PersonalField2:PersonalField10B+PersonalField2:PersonalField13+PersonalField2:PersonalField10A+PersonalField2:PersonalField11+PersonalField2:PersonalField28+PersonalField10B:PersonalField13+PersonalField10B:PersonalField10A+PersonalField10B:PersonalField11+PersonalField10B:PersonalField28+PersonalField13:PersonalField10A+PersonalField13:PersonalField10A+PersonalField13:PersonalField11+PersonalField13:PersonalField28+PersonalField10A:PersonalField11+PersonalField10A:PersonalField28+PersonalField11:PersonalField28


  PersonalField12:PersonalField10B   0.1036322  0.0099006  10.467  < 2e-16 ***
  PersonalField12:PersonalField13   -0.9290442  0.0467395 -19.877  < 2e-16 ***
  PersonalField9:PersonalField13    -1.6757488  0.0901434 -18.590  < 2e-16 ***
  PersonalField1:PersonalField13     3.8549527  0.3937244   9.791  < 2e-16 ***
  PersonalField12:PersonalField9     0.3460621  0.0553163   6.256 3.95e-10 ***
  PersonalField13:PersonalField10A  -0.2311989  0.0407217  -5.678 1.37e-08 ***
  PersonalField12:PersonalField10A   0.0646247  0.0130019   4.970 6.68e-07 ***
  PersonalField2:PersonalField28     0.4468400  0.1144651   3.904 9.47e-05 ***
  PersonalField10B:PersonalField13  -0.0882138  0.0326834  -2.699  0.00695 ** 
  PersonalField1:PersonalField10B   -0.1388266  0.0460403  -3.015  0.00257 ** 
  PersonalField1:PersonalField10A    0.1243282  0.0472363   2.632  0.00849 ** 
  PersonalField2:PersonalField10A   -0.1390245  0.0470443  -2.955  0.00312 ** 
  PersonalField10A:PersonalField11   0.0582288  0.0224267   2.596  0.00942 ** 
  PersonalField10A:PersonalField28  -0.0154635  0.0053334  -2.899  0.00374 **
  PersonalField12:PersonalField11    0.2707970  0.1077733   2.513  0.01198 * 
  PersonalField9:PersonalField2      0.8718345  0.3501148   2.490  0.01277 *
  PersonalField2:PersonalField10B    0.0987323  0.0458428   2.154  0.03126 *
  PersonalField2:PersonalField13    -0.8955863  0.3637132  -2.462  0.01380 *
  PersonalField2:PersonalField10B    0.0987323  0.0458428   2.154  0.03126 * 
  PersonalField2:PersonalField13    -0.8955863  0.3637132  -2.462  0.01380 *
  PersonalField2:PersonalField11     1.1154885  0.5552056   2.009  0.04452 * 
  PersonalField1:PersonalField11    -1.0231194  0.5565663  -1.838  0.06602 .

PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28


### ^-16 to ^-5 plus interactions with 10^-2 of the same type of data

PersonalField19ZN  1.339e+00  1.479e-01   9.056  < 2e-16 *** //partial
PersonalField18YF  2.111e+00  2.211e-01   9.550  < 2e-16 ***  //partial
PersonalField17ZQ  1.508e+00  1.627e-01   9.270  < 2e-16 ***  //partial
PersonalField16ZW  9.854e-01  9.861e-02   9.993  < 2e-16 ***  //partial

GeographicField38B   1.265e-02  3.138e-03   4.030 5.58e-05 ***
  PropertyField33H  -0.117184   0.028043  -4.179 2.93e-05 ***
  PersonalField48    1.125e+00  2.607e-01   4.314 1.60e-05 ***
  PersonalField29   -1.166e+00  2.642e-01  -4.411 1.03e-05 ***
  GeographicField6B    3.128e-02  7.624e-03   4.103 4.07e-05 ***
  GeographicField7A    9.578e-02  2.215e-02   4.325 1.53e-05 ***
  GeographicField12B  -6.314e-02  1.449e-02  -4.358 1.31e-05 ***
  GeographicField20B   1.457e-02  3.563e-03   4.088 4.35e-05 ***
  GeographicField50B   1.911e-02  4.901e-03   3.899 9.64e-05 ***
  GeographicField21B  -1.493e-02  3.565e-03  -4.189 2.80e-05 ***
  GeographicField23B   1.872e-02  4.152e-03   4.509 6.52e-06 ***
  GeographicField47A  -1.749e-02  3.679e-03  -4.754 1.99e-06 ***
  Day13              0.210806   0.047659   4.423 9.72e-06 ***
  SalesField11 -0.122110   0.026442   -4.618 3.87e-06 ***
  SalesField12  0.110561   0.024818    4.455 8.39e-06 ***
  PersonalField43   -3.247e+00  7.181e-01  -4.521 6.16e-06 ***
  GeographicField13A  -1.467e-01  3.279e-02  -4.473 7.71e-06 ***
  GeographicField26A   4.452e-02  9.210e-03   4.834 1.34e-06 ***
  CoverageField3A    0.028412   0.005422   5.240 1.60e-07 ***
  CoverageField2B   -0.034797   0.007018  -4.959 7.10e-07 ***
  PropertyField7F  -1.350e+00  2.552e-01  -5.292 1.21e-07 ***
  GeographicField25A  -8.281e-02  1.671e-02  -4.956 7.18e-07 ***
  GeographicField15B  -2.951e-02  5.510e-03  -5.355 8.53e-08 ***
  CoverageField11A  -0.019956   0.003684  -5.417 6.07e-08 ***
  PropertyField1A  -2.447e-02  4.476e-03  -5.467 4.59e-08 ***
  PersonalField25   -7.794e-01  1.419e-01  -5.495 3.92e-08 ***
  PersonalField22    6.533e-01  1.225e-01   5.331 9.75e-08 ***
  GeographicField47B   1.615e-02  2.989e-03   5.403 6.56e-08 ***
  GeographicField64TX -1.258e+00  2.198e-01  -5.726 1.03e-08 ***
  PropertyField22  -0.559277   0.097445  -5.739 9.50e-09 ***
  PersonalField58   -1.362e+00  2.245e-01  -6.067 1.31e-09 ***
  GeographicField27A   7.415e-02  1.219e-02   6.084 1.18e-09 ***
  PersonalField34   -2.409e+00  3.840e-01  -6.275 3.49e-10 ***
  CoverageField5B   -0.013613   0.002160  -6.301 2.95e-10 ***
  PropertyField12  -3.614e-02  5.881e-03  -6.146 7.95e-10 ***
  PropertyField16A -1.572e-02  2.540e-03  -6.188 6.10e-10 ***
  PersonalField83    7.331e-01  1.182e-01   6.200 5.66e-10 ***
  CoverageField8V    1.384289   0.205759   6.728 1.72e-11 ***
  PropertyField33F  -0.369260   0.056543  -6.531 6.55e-11 ***
  PropertyField18   6.684e-02  9.136e-03   7.316 2.55e-13 ***
  PersonalField6    -1.909e-01  2.737e-02  -6.976 3.05e-12 ***
  

  Field11           -4.011020   0.529324  -7.578 3.52e-14
CoverageField4A   -0.064990   0.008503  -7.643 2.13e-14 ***

  PersonalField14   -3.982e-02  5.339e-03  -7.459 8.74e-14 ***
  PropertyField21B  0.023129   0.003042   7.604 2.86e-14 ***
  PropertyField23  -0.020756   0.002597  -7.994 1.31e-15 ***
  
  PropertyField23+PropertyField21B+PropertyField23:PropertyField21BPersonalField14+CoverageField4A+Field11 


  

####                                  GLM/LOGIT
# build
glm.fit20=glm(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28-PersonalField12-PersonalField1-PersonalField10A-PersonalField28-Month-PropertyField19-PersonalField13:PersonalField10A,data=Traincorr,family=binomial)
summary(glm.fit20)

# predict
glm.prob_21=predict(glm.fitTest,newdata=Testcorr, type="response")   # predict using model
summary(glm.prob_21)
head(glm.prob_21)

#accuracy
table(Testcorr$QuoteConversion_Flag, glm.prob_21 > 0.5)
=TN+TP/(TN+TP+FN+FP)
(50786+5601)/(50786+5601+2179+6623)

# 0.85
# 0.8437466  AIC: 147493 (cumulative)  glm.fit15=glm(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28,data=Traincorr,family=binomial)
# 0.8472902  AIC: 131967 (cumulative) glm(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B,data=Traincorr,family=binomial)
# 0.8154597  AIC: 147476 (subset-PropertyField21A-PropertyField34N-PropertyField28D-PropertyField34Y-PropertyField28B-PropertyField24B) 
# 0.8670788  AIC: 111483 (cumulative) glm.fit15=glm(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B,data=Traincorr,family=binomial)
# score: 0.90768 accuracy: 0.8694872  AIC: 109820 (cum+person ^-16 interactions) glm.fit15=glm(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=binomial)
# score: 0.72760 accuracy: 0.8696252 AIC: 109646 +field+day+sales field (^16-^05) glm.fit19=glm(QuoteConversion_Flag~Field11+Day+SalesField11+SalesField12:SalesField11+PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=binomial)
# AUC= 0.70778, accuracy =0.8649772 AIC: 114096 sig ^16+interact to ^-14best 14-no interact; reg.fit=regsubsets(QuoteConversion_Flag~PropertyField21B+CoverageField4A+Field11+PersonalField9+PersonalField2+PersonalField10B+PersonalField13+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,nvmax=14, data=Traincorr,really.big=T)


#build for accuracy
glm.fit20=glm(QuoteConversion_Flag~PropertyField21B+CoverageField4A+Field11+PersonalField9+PersonalField2+PersonalField10B+PersonalField13+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=binomial)
summary(glm.fit20)

glm.prob_20=predict(glm.fit20,newdata=Testcorr, type="response")   # predict using model
summary(glm.prob_20)

#test variables
glm.fitTest=glm(QuoteConversion_Flag~PropertyField23+PropertyField21B+CoverageField4A+Field11,data=Traincorr,family=binomial)
summary(glm.fitTest)
PersonalField14                  -0.0012544  0.0044563  -0.281    0.778  
PropertyField23:PropertyField21B  0.0004361  0.0003868   1.127    0.260 

#regsubset
install.packages("leaps")
library(leaps)

2 <1min
4 <1min
8  1min        10:34-10:35
12 40 min      10:36-11:20 PersonalField1 PersonalField2 PersonalField10B PersonalField13 CoverageField11B SalesField2A SalesField4 SalesField5 PropertyField35  PersonalField12:PersonalField9, PersonalField1:PersonalField13 PersonalField12:PersonalField10B
14  1 hr 33min 11;27-1:04  PersonalField2 PersonalField10B PersonalField13 Field7 CoverageField6A CoverageField11B SalesField2A SalesField4 SalesField5 SalesField10 PropertyField35 PersonalField12:PersonalField10B PersonalField1:PersonalField13 PersonalField12:PersonalField9    
  

summary(reg.fit)

  
reg.fit=regsubsets(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,nvmax=14, data=Traincorr,really.big=T)
summary(reg.fit)

#regsubset variables
glm.fitTest=glm(QuoteConversion_Flag~PersonalField2+PersonalField10B+PersonalField13+Field7+CoverageField6A+CoverageField11B+SalesField2A+SalesField4+SalesField5+SalesField10+PropertyField35+PersonalField12:PersonalField10B+PersonalField1:PersonalField13+PersonalField12:PersonalField9,data=Traincorr,family=binomial)
summary(glm.fitTest)

#22 Accuracy = 0.8212996 AIC:116102 regsubset=12 (16^+14^) glm.fitTest=glm(QuoteConversion_Flag~PersonalField1+PersonalField2+PersonalField10B+PersonalField13+CoverageField11B+SalesField2A+SalesField4+SalesField5+PropertyField35+PersonalField12:PersonalField9+PersonalField1:PersonalField13+PersonalField12:PersonalField10B,data=Traincorr,family=binomial)
#23 Accuracy = 0.8642102 AIC: 113963 regsubset=14 (16^+14^) glm.fitTest=glm(QuoteConversion_Flag~PersonalField2+PersonalField10B+PersonalField13+Field7+CoverageField6A+CoverageField11B+SalesField2A+SalesField4+SalesField5+SalesField10+PropertyField35+PersonalField12:PersonalField10B+PersonalField1:PersonalField13+PersonalField12:PersonalField9,data=Traincorr,family=binomial)


PersonalField1                   -0.858355   1.228621  -0.699    0.485   

glm.probTest=predict(glm.fitTest,newdata=Testcorr, type="response")

#convert to 1,0's
TestPredictionBinary=ifelse(glm.probTest >0.5,1,0)

#accuracy
table(Testcorr$QuoteConversion_Flag, TestPredictionBinary)
=TN+TP/(TN+TP+FN+FP)
(50775+5562)/(50775+5562+6662+2190)

#[1] 0.8486742  #with significant glm.fit20=glm(QuoteConversion_Flag~PropertyField23+PropertyField21B+CoverageField4A+Field11+PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=binomial)
#accuracy 0.8656368 # #AIC: 109780 [1] less not sig from 2nd ^-15 to^-14 glm.fit20=glm(QuoteConversion_Flag~PropertyField21B+CoverageField4A+Field11+PersonalField9+PersonalField2+PersonalField10B+PersonalField13+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=binomial)

##Submittal

#if N/A =0
#glm.prob_15[is.na(glm.prob_15)] <- 0

#add quote #
test2<-test$QuoteNumber

#quote number as integer
test3=as.integer(test2)

glm.prob_23=predict(glm.fitTest,newdata=test, type="response")
TestPredictionBinary23=ifelse(glm.prob_23 >0.5,1,0)
glm.prob_23=cbind(TestPredictionBinary23, test3)

write.csv(glm.prob_23,"glm.prob_23.csv")

### logistic regression parameters

glm(formula, family = gaussian, data, weights, subset,
    na.action, start = NULL, etastart, mustart, offset,
    control = list(...), model = TRUE, method = "glm.fit",
    x = FALSE, y = TRUE, contrasts = NULL, ...)
#---family ---
#binomial(link = "logit")
#gaussian(link = "identity")
#Gamma(link = "inverse")
#inverse.gaussian(link = "1/mu^2")
#poisson(link = "log")
#quasi(link = "identity", variance = "constant")
#quasibinomial(link = "logit")
#quasipoisson(link = "log")


### Logit model turning
# 0.8588719AIC: 104027 (gaussian+(cum+person ^-16 interactions) glm.fit18=glm(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=gaussian)

### Step *** need Spark for n>10 
# 0.8749861  AIC=113266  (gaussian+cum+person ^-16 interactions + regsubsets (8 variables) reg.fit=regsubsets(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,really.big=T)
#         glm.fit17=glm(QuoteConversion_Flag~SalesField5+PersonalField2+PersonalField10B+PersonalField13+CoverageField11B+SalesField2A+PersonalField1:PersonalField13+PersonalField12:PersonalField9,data=Traincorr,family=gaussian)
# 0.8830365 AIC: 111025 (gaussian+cum+person ^-16 interactions + regsubsets (10 variables-15 min) reg.fit=regsubsets(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,nvmax=10,really.big=T)
#         glm.fit17=glm(QuoteConversion_Flag~PersonalField1:PersonalField13+PersonalField12:PersonalField9+PropertyField35+CoverageField11B+SalesField2A+SalesField4+SalesField5+PersonalField2+PersonalField10B+PersonalField13,data=Traincorr,family=gaussian)


##10-fold *** need Spark for n>8// features from best model=# 0.8694872  AIC: 109820

### 10-fold *** na.omit(train), features from best model=# 0.8694872  AIC: 109820 

str(train)
#260753 obs. of  303 variables
train=na.omit(train) 
#55856 obs. of  303 variables: 
with(train,sum(is.na(train)))

k=10
set.seed(1)
folds=sample(1:k,nrow(train),replace=TRUE)
folds
table(folds)

cv.errors=matrix(NA,k,8, dimnames=list(NULL, paste(1:8)))

for(k in 1:10){
  best.fit=regsubsets(QuoteConversion_Flag~~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B+PropertyField21A+PropertyField34+PropertyField28+PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=train[folds!=k,],method="forward")
  for(i in 1:8){
    pred=predict(best.fit,train[folds==k,],id=i)
    cv.errors[k,i]=mean( (train$QuoteConversion_Flag[folds==k]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)


###Ridge (glmnet) alpha=0

library(glmnet)
#matrix x-of predictors
x=model.matrix(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28-1,data=Traincorr)
#matrix y- of repsonse
y=Traincorr$QuoteConversion_Flag
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0) #11:19 - 11:20
plot(cv.ridge) 

###Lasso (glmnet) alpha=1
x=model.matrix(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28-1,data=Traincorr)
#matrix y- of repsonse
y=Traincorr$QuoteConversion_Flag
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y,alpha=0) #11:24 - 
plot(cv.ridge) 
coef(cv.lasso)
lasso.tr=glmnet(x[train],y[train])
lasso.tr
pred=predict(lasso.tr x[-train,])
dim(pred)
rmse=sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")

##Elastic Net model (glmnet) alpha=0,1


##        Bestsubset selection

library(leaps)
reg.fit=regsubsets(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,nvmax=10,really.big=T)
#10:20-10:34
summary(reg.fit)

PersonalField1:PersonalField13+PersonalField12:PersonalField9+PropertyField35+CoverageField11B+SalesField2A+SalesField4+SalesField5+PersonalField2+PersonalField10B+PersonalField13
glm.fit17=glm(QuoteConversion_Flag~PersonalField1:PersonalField13+PersonalField12:PersonalField9+PropertyField35+CoverageField11B+SalesField2A+SalesField4+SalesField5+PersonalField2+PersonalField10B+PersonalField13,data=Traincorr,family=binomial)

summary(glm.fit17)
reg.summary=summary(glm.fit17)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)                         #which # gives lowest error
points(10,reg.summary$cp[10],pch=20, col="red")   #if 10 gives the lowest error
plot(reg.fit17,scale="Cp")
coef(regfit.full,10)

glm.prob_18=predict(glm.fit18,newdata=Testcorr, type="response")

TestPredictionBinary18=ifelse(glm.prob_18 >0.5,1,0)

#accuracy
table(Testcorr$QuoteConversion_Flag, TestPredictionBinary18)
=TN+TP/(TN+TP+FN+FP)
(52236+3753)/(52236+3753+8471+729)

####        Forward selection (Greedy)
method="forward"
reg.fit=regsubsets(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,method="forward",nvmax=10,really.big=T)

plot(reg.fit,scale="Cp")
val.errors=rep(NA,19)              #set up vector for nvmax=?  i.e. 19 slots
#xmatrix corresponding to the validation set







#  -  CART  - - - 
# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

homeSiteTree15 = rpart(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField10B+PersonalField1+PersonalField2+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B, data = Traincorr, method="class", minbucket=200)

#PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B
#-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B-PersonalField12-PersonalField1-PersonalField10A-PersonalField28-Month-PropertyField19

prp(homeSiteTree15)
PredictCART15 = predict(homeSiteTree15, newdata = Testcorr, type = "class")  
table(PredictCART15)
table(Testcorr$QuoteConversion_Flag)

#accuracy
table(Testcorr$QuoteConversion_Flag, PredictCART15)
=TN+TP/(TN+TP+FN+FP)
(50120+6407)/(50120+6407+5817+2845)

# 0.8671248 >> minibucket=25 homeSiteTree15 = rpart(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField10B+PersonalField1+PersonalField2+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B, data = Traincorr, method="class", minbucket=25)

PredictCART15_S = predict(homeSiteTree15, newdata = Testcorr, type = "class")

glm.prob_15=cbind(PredictCART15_S, test3)

write.csv(glm.prob_15,"glm.prob_15.csv")


####                                RANDOM FOREST
install.packages("randomForest")
library(randomForest)

# build

StevensForest = randomForest(QuoteConversion_Flag ~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B, data =Traincorr,  ntree=200, nodesize=25 )
summary(StevensForest)
str(StevensForest)

# predict
PredictForest = predict(StevensForest, newdata = Testcorr)

#accuracy
table(Testcorr$QuoteConversion_Flag, PredictForest > 0.5)
=TN+TP/(TN+TP+FN+FP)
(52830+2523)/(52830+2523+135+9701)
#0.85
#[1] 0.8491156 #StevensForest = randomForest(QuoteConversion_Flag ~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28, data =Traincorr,  ntree=200, nodesize=25 )





### shrinkage panalty (Lasso & Ridge Regression/ Regularization)

# Ridge lambda*absolute (least sq.) (l2 penalty) tuning parameter by cross-validation /// penalize coefficients that get too large, to zero
# cross validation for regularization parameters
# Select the k=5 or 10-fold that provides the smallest error


# Lasso lambda*absolute (least sq.) (l1 penalty) does subset selection i.e. set features to zero
# cross validation for regularization parameters
# Select the k=5 or 10-fold that provides the smallest error





 
  
##feature engineer

test
glm.fit19=glm(QuoteConversion_Flag~Field11+Day+SalesField11+SalesField12:SalesField11,data=Traincorr,family=binomial)
summary(glm.fit19)

#significant vars
Field11+Day+SalesField11+SalesField12:SalesField11

#updated
glm.fit19=glm(QuoteConversion_Flag~Field11+Day+SalesField11+SalesField12:SalesField11+PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=binomial)
summary(glm.fit19)
# 0.8696252 AIC: 109646 +field+day+sales field (^16-^05) glm.fit19=glm(QuoteConversion_Flag~Field11+Day+SalesField11+SalesField12:SalesField11+PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,family=binomial)

#reg.fit=regsubsets(QuoteConversion_Flag~PersonalField12+PersonalField9+PersonalField1+PersonalField2+PersonalField10B+PersonalField13+PersonalField10A+PersonalField11+PersonalField28+Month+CoverageField3B+CoverageField3B+CoverageField5A+Field7+CoverageField6A+CoverageField6B+CoverageField8+CoverageField11B+SalesField2A+SalesField3+SalesField4+SalesField5+SalesField6+SalesField9+SalesField10+SalesField13+PropertyField1B+PropertyField2A+PropertyField2B+PropertyField8+PropertyField13+PropertyField15+PropertyField16B+PropertyField17+PropertyField19+PropertyField21A+PropertyField24A+PropertyField24B+PropertyField25+PropertyField26A+PropertyField26B+PropertyField28+PropertyField30+PropertyField33+PropertyField34+PropertyField35+PropertyField39B-PropertyField21A-PropertyField34-PropertyField28-PropertyField24B+PersonalField12:PersonalField10B+PersonalField12:PersonalField13+PersonalField9:PersonalField13+PersonalField1:PersonalField13+PersonalField12:PersonalField9+PersonalField13:PersonalField10A+PersonalField12:PersonalField10A+PersonalField2:PersonalField28,data=Traincorr,really.big=T)
#summary(reg.fit)

# check interaction
# all
# deleted 2nd sign
# deleted 1st sign

glm.prob_19=predict(glm.fit19,newdata=Testcorr, type="response")

TestPredictionBinary19=ifelse(glm.prob_19 >0.5,1,0)

#accuracy
table(Testcorr$QuoteConversion_Flag, TestPredictionBinary19)
=TN+TP/(TN+TP+FN+FP)
(50687 +6003)/(50687 +6003+2278+6221)

#add quote #
test2<-test$QuoteNumber

#quote number as integer
test3=as.integer(test2)

glm.prob_19=predict(glm.fit19,newdata=test, type="response")
TestPredictionBinary19=ifelse(glm.prob_19 >0.5,1,0)
glm.prob_19=cbind(TestPredictionBinary19, test3)

write.csv(glm.prob_19,"glm.prob_19.csv")



  GeographicField38B
  GeographicField6B
  GeographicField7A
  GeographicField12B
  GeographicField20B
  GeographicField50B
  GeographicField21B
  GeographicField23B
  GeographicField47A
  GeographicField13A
  GeographicField26A
  GeographicField25A 
  GeographicField15B
  GeographicField47B
  GeographicField64TX
  GeographicField27A  
  
  PersonalField43  
  PersonalField48  
  PersonalField29  
  PersonalField25   
  PersonalField22  
  PersonalField58  
  PersonalField34  
  PersonalField83  
  PersonalField6   
  PersonalField14 
  PersonalField19ZN 
PersonalField18YF  
PersonalField17ZQ  
PersonalField16ZW   

  PropertyField33H 
  PropertyField7F
  PropertyField1A 
  PropertyField22 
  PropertyField12 
  PropertyField16A
  PropertyField33F 
  PropertyField18  
  PropertyField21B 
  PropertyField23  
  
  CoverageField3A
  CoverageField2B
  CoverageField11A
  CoverageField5B
  CoverageField8V 
  CoverageField4A  

  
  Field11+Day13+SalesField11+SalesField12+SalesField12:SalesField11
  

  
# Chapter 6 Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

