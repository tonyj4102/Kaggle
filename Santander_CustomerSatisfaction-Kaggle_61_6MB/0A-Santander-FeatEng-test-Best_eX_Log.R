#create base train and test data frames
library(dplyr)

FETrain<-NULL
FETest<-NULL

FETrain<-select(Strain, 
                ID, TARGET)
FETest<-select(Stest, 
               ID)

# add variables by absolute feature importance

#cluster	0.960151109	0.478102444
FETrain$cluster<-XTrain$cluster
FETest$cluster<-XTest$cluster
FETrain$cluster<-as.numeric(FETrain$cluster)
FETest$cluster<-as.numeric(FETest$cluster)
#[14]	train-auc:0.556108+0.009044	test-auc:0.504667+0.016212

#cluster	0.960151109	0.478102444
FETrain$cluster5<-XTrain$cluster5
FETest$cluster5<-XTest$cluster5
FETrain$cluster5<-as.numeric(FETrain$cluster5)
FETest$cluster5<-as.numeric(FETest$cluster5)

#features

saldo_var29	0.408537169	0.301430163   
FETrain$saldo_var29 <-Strain$saldo_var29
FETest$saldo_var29  <-Stest$saldo_var29

saldo_var30	0.334232098	0.254501365  log          
FETrain$saldo_var30 <-Strain$saldo_var30)  # transform and add

MEtrain <- merge(ETrain, StrainEX, by.x = "ID", by.y = "ID")
MEtest <- merge(ETest, StestEX, by.x = "ID", by.y = "ID")
XTrain$cluster
XTrain$cluster


#create base train and test data frames
library(dplyr)

FETrain<-NULL
FETest<-NULL

FETrain<-select(Strain, 
                ID, TARGET)
FETest<-select(Stest, 
               ID)

# add variables by absolute feature importance

#cluster	0.960151109	0.478102444
FETrain$cluster<-XTrain$cluster
FETest$cluster<-XTest$cluster
FETrain$cluster<-as.numeric(FETrain$cluster)
FETest$cluster<-as.numeric(FETest$cluster)
#[14]	train-auc:0.556108+0.009044	test-auc:0.504667+0.016212

#cluster	0.960151109	0.478102444
FETrain$cluster5<-XTrain$cluster5
FETest$cluster5<-XTest$cluster5
FETrain$cluster5<-as.numeric(FETrain$cluster5)
FETest$cluster5<-as.numeric(FETest$cluster5)


saldo_var29	0.408537169	0.301430163  ex                          #  +0.00      @50.0
FETrain$saldo_var29 <-exp(Strain$saldo_var29)
FETest$saldo_var29  <-exp(Stest$saldo_var29)

saldo_var30	0.334232098	0.254501365  log                         #  +0.21      @.21
FETrain$saldo_var30 <-log(Strain$saldo_var30)  #: NaNs produced
FETest$saldo_var30  <-log(Stest$saldo_var30)   #: NaNs produced
var15	0.285352154	0.229799854 base                               #  +0.10      0.10 0.811
FETrain$var15 <-Strain$var15
FETest$var15  <-Stest$var15
var38	0.082425655	0.066379045 base                               #  +0.01      0.08  0.819
FETrain$var38 <-Strain$var38
FETest$var38  <-Stest$var38
saldo_medio_var5_hace2	0.082488761	0.060862517                  #  +0.037     0.03  0.8224
FETrain$saldo_medio_var5_hace2 <-exp(Strain$saldo_medio_var5_hace2)
FETest$saldo_medio_var5_hace2  <-exp(Stest$saldo_medio_var5_hace2)
saldo_medio_var5_hace3	0.063380813	0.048261383 log(                           #0.0004  0.822894
  FETrain$saldo_medio_var5_hace3 <-log(Strain$saldo_medio_var5_hace3) #: NaNs produced
  FETest$saldo_medio_var5_hace3  <-log(Stest$saldo_medio_var5_hace3) #: NaNs produced
  imp_venta_var44_ult1	0.052052146	0.03840553 ex                                #0.000   0.822894
  FETrain$imp_venta_var44_ult1 <-exp(Strain$imp_venta_var44_ult1)
  FETest$imp_venta_var44_ult1  <-exp(Stest$imp_venta_var44_ult1)      
  Cluster   #0.0002  #0.823083
  

      
FETest$saldo_var30  <-Stest$saldo_var30)   #: NaNs produced
var15	0.285352154	0.229799854 base                               #  +0.10      0.10 0.811
FETrain$var15 <-Strain$var15
FETest$var15  <-Stest$var15
var38	0.082425655	0.066379045 base                               #  +0.01      0.08  0.819
FETrain$var38 <-Strain$var38
FETest$var38  <-Stest$var38
saldo_medio_var5_hace2	0.082488761	0.060862517                  #  +0.037     0.03  0.8224
FETrain$saldo_medio_var5_hace2 <-exp(Strain$saldo_medio_var5_hace2)
FETest$saldo_medio_var5_hace2  <-exp(Stest$saldo_medio_var5_hace2)
saldo_medio_var5_hace3	0.063380813	0.048261383 log(                           #0.0004  0.822894
  FETrain$saldo_medio_var5_hace3 <-log(Strain$saldo_medio_var5_hace3) #: NaNs produced
  FETest$saldo_medio_var5_hace3  <-log(Stest$saldo_medio_var5_hace3) #: NaNs produced
  imp_venta_var44_ult1	0.052052146	0.03840553 ex                                #0.000   0.822894
  FETrain$imp_venta_var44_ult1 <-exp(Strain$imp_venta_var44_ult1)
  FETest$imp_venta_var44_ult1  <-exp(Stest$imp_venta_var44_ult1)      
  Cluster   #0.0002  #0.823083
  
  #ALL  0.831377
  
  
  saldo_medio_var5_ult1	0.042848259	0.031614646 ex                        0.0004  0.823508
  FETrain$saldo_medio_var5_ult1 <-exp(Strain$saldo_medio_var5_ult1)
  FETest$saldo_medio_var5_ult1  <-exp(Stest$saldo_medio_var5_ult1)  
  imp_op_var40_comer_ult3	0.039121319	0.028864804 ex                      0.0004  0.8239
  FETrain$imp_op_var40_comer_ult3 <-exp(Strain$imp_op_var40_comer_ult3)
  FETest$imp_op_var40_comer_ult3  <-exp(Stest$imp_op_var40_comer_ult3) 
  imp_op_var41_efect_ult1	0.036531801	0.026954186 ex                     0.003  0.8258
  FETrain$imp_op_var41_efect_ult1 <-exp(Strain$imp_op_var41_efect_ult1)
  FETest$imp_op_var41_efect_ult1  <-exp(Stest$imp_op_var41_efect_ult1)
  saldo_medio_var5_ult3	0.03397831	0.025872818  log                     -0.000  0.8258
  FETrain$saldo_medio_var5_hace3 <-log(Strain$saldo_medio_var5_hace3) #: NaNs produced
  FETest$saldo_medio_var5_hace3  <-log(Stest$saldo_medio_var5_hace3) #: NaNs produced
  
  # [31]	train-auc:0.878794+0.002564	test-auc:0.824074+0.015835
  
  imp_sal_var16_ult1	0.031740841	0.023419281 example                    -0.001 0.824201
  FETrain$imp_sal_var16_ult1 <-exp(Strain$imp_sal_var16_ult1)
  FETest$imp_sal_var16_ult1  <-exp(Stest$imp_sal_var16_ult1)
  
  saldo_var6	0.022402937	0.016529514 ex                                 0.0014.825674
  FETrain$saldo_var6 <-exp(Strain$saldo_var6)
  FETest$saldo_var6  <-exp(Stest$saldo_var6)
  
  saldo_var33	0.021911836	0.016167166 ex
  
  FETrain$saldo_var33 <-exp(Strain$saldo_var33)
  FETest$saldo_var33  <-exp(Stest$saldo_var33)  
  
  imp_op_var41_efect_ult3	0.027173799	0.020691516 log
  FETrain$imp_op_var41_efect_ult3 <-log(Strain$imp_op_var41_efect_ult3) 
  FETest$imp_op_var41_efect_ult3  <-log(Stest$imp_op_var41_efect_ult3)
  
  saldo_var5	0.02535016	0.019302905 log
  FETrain$saldo_var5 <-log(Strain$saldo_var5) #: NaNs produced
  FETest$saldo_var5  <-log(Stest$saldo_var5) #: NaNs produced
  
  saldo_var37	0.025244444	0.019222407 log
  FETrain$saldo_var37 <-log(Strain$saldo_var37)
  FETest$saldo_var37  <-log(Stest$saldo_var37) 
  
  saldo_var1	0.020444019	0.01508417 ex
  FETrain$saldo_var1 <-exp(Strain$saldo_var1)
  FETest$saldo_var1  <-exp(Stest$saldo_var1)
  
  imp_op_var41_ult1	0.019000616	0.014468038 log
  FETrain$imp_op_var41_ult1 <-log(Strain$imp_op_var41_ult1) 
  FETest$imp_op_var41_ult1  <-log(Stest$imp_op_var41_ult1) 
  
  saldo_var8	0.017489087	0.013317083 log
  FETrain$saldo_var8 <-log(Strain$saldo_var8) #: NaNs produced
  FETest$saldo_var8  <-log(Stest$saldo_var8) #: NaNs produced
  
  num_var45_hace3	0.015344297	0.012357073 base
  FETrain$num_var45_hace3 <-Strain$num_var45_hace3
  FETest$num_var45_hace3  <-Stest$num_var45_hace3
  
  imp_trans_var37_ult1	0.015907097	0.012112475 log
  FETrain$imp_trans_var37_ult1 <-log(Strain$imp_trans_var37_ult1) #
  FETest$imp_trans_var37_ult1  <-log(Stest$imp_trans_var37_ult1) #:
  
  imp_op_var39_comer_ult1	0.015704611	0.011958292 log
  FETrain$imp_op_var39_comer_ult1 <-log(Strain$imp_op_var39_comer_ult1) #: 
  FETest$imp_op_var39_comer_ult1  <-log(Stest$imp_op_var39_comer_ult1) #: 
  
  imp_var43_emit_ult1	0.015631791	0.011902843 log
  FETrain$imp_var43_emit_ult1 <-log(Strain$imp_var43_emit_ult1) #: 
  FETest$imp_var43_emit_ult1 <-log(Stest$imp_var43_emit_ult1) #: 
  
  imp_op_var39_comer_ult3	0.014739696	0.011223556 log
  FETrain$imp_op_var39_comer_ult3 <-log(Strain$imp_op_var39_comer_ult3) #: 
  FETest$imp_op_var39_comer_ult3  <-log(Stest$imp_op_var39_comer_ult3) #: 
  
  imp_ent_var16_ult1	0.013738859	0.010461468 log
  FETrain$imp_ent_var16_ult1 <-log(Strain$imp_ent_var16_ult1) #: 
  FETest$imp_ent_var16_ult1  <-log(Stest$imp_ent_var16_ult1) #: 
  
  # cv= [40] 0.832171+0.011661 = AUC 
  
  
  #  #### more to add
  base
  num_var22_ult1	0.012227565	0.009847105
  FETrain$num_var22_ult1 <-Strain$num_var22_ult1
  FETest$num_var22_ult1  <-Stest$num_var22_ult1
  num_var45_hace2	0.012046816	0.009701544
  FETrain$num_var45_hace2 <-Strain$num_var45_hace2
  FETest$num_var45_hace2  <-Stest$num_var45_hace2
  
  
  
  ex
  imp_reemb_var17_ult1	0.011671178	0.008611322
  FETrain$imp_reemb_var17_ult1 <-exp(Strain$imp_reemb_var17_ult1)
  FETest$imp_reemb_var17_ult1  <-exp(Stest$imp_reemb_var17_ult1) 
  
  imp_op_var40_efect_ult1	0.009046757	0.006674951
  FETrain$imp_op_var40_efect_ult1 <-exp(Strain$imp_op_var40_efect_ult1)
  FETest$imp_op_var40_efect_ult1  <-exp(Stest$imp_op_var40_efect_ult1) 
  
  
  log(
    imp_op_var39_ult1	0.010469341	0.00797189
    FETrain$imp_op_var39_ult1 <-log(Strain$imp_op_var39_ult1) #: 
    FETest$imp_op_var39_ult1  <-log(Stest$imp_op_var39_ult1) #: 
    
    imp_op_var41_comer_ult3	0.009794812	0.007458269
    FETrain$imp_op_var41_comer_ult3 <-log(Strain$imp_op_var41_comer_ult3) #: 
    FETest$imp_op_var41_comer_ult3  <-log(Stest$imp_op_var41_comer_ult3) #: 
    
    
    base
    var3	0.0088686	0.007142063
    FETrain$var3 <-Strain$var3
    FETest$var3  <-Stest$var3
    #
    
    num_meses_var39_vig_ult3	0.00884813	0.007125578
    FETrain$num_meses_var39_vig_ult3 <-Strain$num_meses_var39_vig_ult3
    FETest$num_meses_var39_vig_ult3 <-Stest$num_meses_var39_vig_ult3
    
    num_var45_ult1	0.008457777	0.006811219
    FETrain$num_meses_var39_vig_ult3 <-Strain$num_meses_var39_vig_ult3
    FETest$num_meses_var39_vig_ult3  <-Stest$num_meses_var39_vig_ult3
    
    
    log(
      saldo_medio_var8_ult1	0.008409642	0.00640353
      FETrain$saldo_medio_var8_ult1 <-log(Strain$saldo_medio_var8_ult1) #: 
      FETest$saldo_medio_var8_ult1  <-log(Stest$saldo_medio_var8_ult1) #: 
      
      ex
      saldo_medio_var8_hace3	0.008714312	0.006429664
      FETrain$saldo_medio_var8_hace3 <-exp(Strain$saldo_medio_var8_hace3)
      FETest$saldo_medio_var8_hace3  <-exp(Stest$saldo_medio_var8_hace3) 
      
      base
      num_var22_ult3	0.007802056	0.006283154
      FETrain$num_var22_ult3 <-Strain$num_var22_ult3
      FETest$num_var22_ult3  <-Stest$num_var22_ult3
      
      imp_op_var41_ult1	0.008292319	0.006677972
      FETrain$imp_op_var41_ult1 <-Strain$imp_op_var41_ult1
      FETest$imp_op_var41_ult1  <-Stest$imp_op_var41_ult1
      
      
      num_var22_hace3	0.007756094	0.006246139
      FETrain$num_var22_hace3 <-Strain$num_var22_hace3
      FETest$num_var22_hace3  <-Stest$num_var22_hace3
      
      num_var45_ult3	0.007362684	0.005929318
      FETrain$num_var45_ult3 <-Strain$num_var45_ult3
      FETest$num_var45_ult3  <-Stest$num_var45_ult3
      
      num_var22_hace2	0.006891269	0.005549678
      FETrain$num_var22_hace2 <-Strain$num_var22_hace2
      FETest$num_var22_hace2 <-Stest$num_var22_hace2
      
      imp_ent_var16_ult1	0.006306233	0.005078536
      FETrain$imp_ent_var16_ult1 <-Strain$imp_ent_var16_ult1
      FETest$imp_ent_var16_ult1  <-Stest$imp_ent_var16_ult1
      
      
      ex
      imp_op_var40_comer_ult1	0.005711661	0.004214223
      FETrain$imp_op_var40_comer_ult1 <-exp(Strain$imp_op_var40_comer_ult1)
      FETest$imp_op_var40_comer_ult1  <-exp(Stest$imp_op_var40_comer_ult1) 
      
      imp_op_var41_comer_ult1	0.006675874	0.004925647
      FETrain$imp_op_var41_comer_ult1 <-exp(Strain$imp_op_var41_comer_ult1)
      FETest$imp_op_var41_comer_ult1  <-exp(Stest$imp_op_var41_comer_ult1) 
      
      log
      saldo_var42	0.005921945	0.004509271
      FETrain$saldo_var42 <-log(Strain$saldo_var42) #: 
      FETest$saldo_var42  <-log(Stest$saldo_var42) #: 
      
      ex
      saldo_medio_var8_ult3	0.005744673	0.004238581
      FETrain$saldo_medio_var8_ult3 <-exp(Strain$saldo_medio_var8_ult3)
      FETest$saldo_medio_var8_ult3  <-exp(Stest$saldo_medio_var8_ult3) 
      
      base
      num_var4	0.005635426	0.004538323
      FETrain$num_var4 <-Strain$num_var4
      FETest$num_var4  <-Stest$num_var4
      
      num_med_var45_ult3	0.005334212	0.004295749
      FETrain$num_med_var45_ult3 <-Strain$num_med_var45_ult3
      FETest$num_med_var45_ult3  <-Stest$num_med_var45_ult3
      
      saldo_var13_largo	0.004232877	0.003123135
      FETrain$saldo_var13_largo <-Strain$saldo_var13_largo
      FETest$saldo_var13_largo  <-Stest$saldo_var13_largo
      
      
      log
      saldo_var26	0.004622477	0.00351979
      FETrain$saldo_var26 <-log(Strain$saldo_var26) #: 
      FETest$saldo_var26  <-log(Stest$saldo_var26) #: 
      
      ex
      saldo_medio_var8_hace2	0.004611229	0.003402294
      FETrain$saldo_medio_var8_hace2 <-exp(Strain$saldo_medio_var8_hace2)
      FETest$saldo_medio_var8_hace2  <-exp(Stest$saldo_medio_var8_hace2) 
      
      base
      num_op_var41_efect_ult3	0.003915086	0.003152898
      FETrain$um_op_var41_efect_ult3 <-Strain$um_op_var41_efect_ult3
      FETest$um_op_var41_efect_ult3  <-Stest$um_op_var41_efect_ult3
      
      num_op_var41_ult3	0.003904475	0.003144353
      FETrain$num_op_var41_ult3 <-Strain$num_op_var41_ult3
      FETest$num_op_var41_ult3  <-Stest$num_op_var41_ult3
      
      num_var35	0.003784114	0.003047423
      FETrain$num_var35 <-Strain$num_var35
      FETest$num_var35  <-Stest$num_var35
      
      log
      saldo_var25	0.003396538	0.002586297
      FETrain$saldo_var25 <-log(Strain$saldo_var25) #: 
      FETest$saldo_var25  <-log(Stest$saldo_var25) #: 
      
      base
      num_var42_0	0.003182915	0.002563266
      FETrain$num_var42_0 <-Strain$num_var42_0
      FETest$num_var42_0  <-Stest$num_var42_0
      
      var36	0.002915221	0.002347686
      
      log
      imp_op_var39_efect_ult1	0.002886919	0.002198247
      FETrain$imp_op_var39_efect_ult1 <-log(Strain$imp_op_var39_efect_ult1) #: 
      FETest$imp_op_var39_efect_ult1  <-log(Stest$imp_op_var39_efect_ult1) #: 
      
      
      base
      ind_var8_0	0.00252988	0.002037363
      FETrain$ind_var8_0 <-Strain$ind_var8_0
      FETest$ind_var8_0  <-Stest$ind_var8_0
      
      
      ex
      imp_amort_var34_ult1	0.002673471	0.001972562
      FETrain$imp_amort_var34_ult1 <-exp(Strain$imp_amort_var34_ult1)
      FETest$imp_amort_var34_ult1  <-exp(Stest$imp_amort_var34_ult1) 
      
      delta_imp_amort_var34_1y3	0.002511349	0.001852943
      FETrain$delta_imp_amort_var34_1y3 <-exp(Strain$delta_imp_amort_var34_1y3)
      FETest$delta_imp_amort_var34_1y3  <-exp(Stest$delta_imp_amort_var34_1y3) 
      
      imp_var7_emit_ult1	0.002144821	0.001582509
      FETrain$imp_var7_emit_ult1 <-exp(Strain$imp_var7_emit_ult1)
      FETest$imp_var7_emit_ult1  <-exp(Stest$imp_var7_emit_ult1) 
      
      base
      num_op_var41_efect_ult1	0.002360237	0.001900747
      FETrain$num_op_var41_efect_ult1 <-Strain$num_op_var41_efect_ult1
      FETest$num_op_var41_efect_ult1  <-Stest$num_op_var41_efect_ult1
      
      num_var37_med_ult2	0.00225558	0.001816464
      FETrain$num_var37_med_ult2 <-Strain$num_var37_med_ult2
      FETest$num_var37_med_ult2  <-Stest$num_var37_med_ult2
      
      log
      imp_op_var39_efect_ult3	0.002231631	0.001699278
      FETrain$imp_op_var39_efect_ult3 <-log(Strain$imp_op_var39_efect_ult3) #: 
      FETest$imp_op_var39_efect_ult3  <-log(Stest$imp_op_var39_efect_ult3) #: 
      
      base 
      num_med_var22_ult3	0.002189357	0.001763134
      FETrain$num_med_var22_ult3 <-Strain$num_med_var22_ult3
      FETest$num_med_var22_ult3  <-Stest$num_med_var22_ult3
      
      num_op_var41_hace2	0.002018871	0.001625838
      FETrain$num_op_var41_hace2 <-Strain$num_op_var41_hace2
      FETest$num_op_var41_hace2  <-Stest$num_op_var41_hace2
      
      num_op_var39_ult1	0.0019494	0.001569891
      FETrain$num_op_var39_ult1 <-Strain$num_op_var39_ult1
      FETest$num_op_var39_ult1  <-Stest$num_op_var39_ult1
      
      num_op_var41_ult1	0.001785698	0.001438059
      FETrain$num_op_var41_ult1 <-Strain$num_op_var41_ult1
      FETest$num_op_var41_ult1  <-Stest$num_op_var41_ult1
      
      log
      saldo_medio_var12_hace2	0.001728707	0.001316326
      FETrain$saldo_medio_var12_hace2 <-log(Strain$saldo_medio_var12_hace2) #: 
      FETest$saldo_medio_var12_hace2  <-log(Stest$saldo_medio_var12_hace2) #: 
      
      ex
      num_op_var39_comer_ult1	0.002219342	0.001787281
      FETrain$num_op_var39_comer_ult1 <-exp(Strain$snum_op_var39_comer_ult1)
      FETest$num_op_var39_comer_ult1  <-exp(Stest$num_op_var39_comer_ult1) 
      
      num_op_var39_comer_ult3	0.001706123	0.001373975
      FETrain$num_op_var39_comer_ult3 <-exp(Strain$num_op_var39_comer_ult3)
      FETest$num_op_var39_comer_ult3  <-exp(Stest$num_op_var39_comer_ult3) 
      
      num_var43_recib_ult1	0.001683852	0.00135604
      FETrain$um_var43_recib_ult1 <-exp(Strain$um_var43_recib_ult1)
      FETest$um_var43_recib_ult1  <-exp(Stest$um_var43_recib_ult1) 
      
      num_var30_0	0.001509472	0.001215608
      FETrain$num_var30_0 <-exp(Strain$num_var30_0)
      FETest$num_var30_0  <-exp(Stest$num_var30_0) 
      
      num_ent_var16_ult1	0.001382285	0.001113182
      FETrain$num_ent_var16_ult1 <-exp(Strain$num_ent_var16_ult1)
      FETest$num_ent_var16_ult1  <-exp(Stest$num_ent_var16_ult1) 
      
      ind_var39_0	0.001308889	0.001054074
      FETrain$ind_var39_0 <-exp(Strain$ind_var39_0)
      FETest$ind_var39_0  <-exp(Stest$ind_var39_0) 
      
      #  cv AUC: train-auc:0.904448+0.001487	test-auc:0.835395+0.013301
      #  test AUC 0.833888
      
      #  +all train
      #  +cluster
      #  +grid search
      
      
      log
      imp_op_var40_ult1	0.001301606	0.000991109
      FETrain$imp_op_var40_ult1 <-log(Strain$imp_op_var40_ult1) #: 
      FETest$imp_op_var40_ult1  <-log(Stest$imp_op_var40_ult1) #:
      
      base
      num_op_var41_comer_ult1	0.001211135	0.000975352
      FETrain$num_op_var41_comer_ult1 <-Strain$num_op_var41_comer_ult1
      FETest$num_op_var41_comer_ult1  <-Stest$num_op_var41_comer_ult1
      
      num_meses_var5_ult3	0.001125754	0.000906593
      FETrain$num_meses_var5_ult3 <-Strain$num_meses_var5_ult3
      FETest$num_meses_var5_ult3  <-Stest$num_meses_var5_ult3
      
      ex
      saldo_var40	0.001169378	0.0008628
      FETrain$saldo_var40 <-exp(Strain$saldo_var40)
      FETest$saldo_var40  <-exp(Stest$saldo_var40) 
      
      base
      num_meses_var8_ult3	0.001089858	0.000877685
      FETrain$num_meses_var8_ult3 <-Strain$num_meses_var8_ult3
      FETest$num_meses_var8_ult3  <-Stest$num_meses_var8_ult3
      
      log
      saldo_medio_var13_corto_hace2	0.001088707	0.000828997
      FETrain$saldo_medio_var13_corto_hace2 <-log(Strain$saldo_medio_var13_corto_hace2) #: 
      FETest$saldo_medio_var13_corto_hace2  <-log(Stest$saldo_medio_var13_corto_hace2) #:
      
      
      base
      num_op_var41_hace3	0.000995184	0.000801442
      FETrain$num_op_var41_hace3 <-Strain$num_op_var41_hace3
      FETest$num_op_var41_hace3  <-Stest$num_op_var41_hace3
      
      imp_aport_var13_hace3	0.000966667	0.000778477
      FETrain$imp_aport_var13_hace3 <-Strain$imp_aport_var13_hace3
      FETest$imp_aport_var13_hace3  <-Stest$imp_aport_var13_hace3
      
      log
      imp_op_var40_efect_ult3	0.000949976	0.00072336
      FETrain$imp_op_var40_efect_ult3 <-log(Strain$imp_op_var40_efect_ult3) #: 
      FETest$imp_op_var40_efect_ult3  <-log(Stest$imp_op_var40_efect_ult3) #:
      
      saldo_medio_var12_ult1	0.000926473	0.000705464
      FETrain$saldo_medio_var12_ult1 <-log(Strain$saldo_medio_var12_ult1) #: 
      FETest$saldo_medio_var12_ult1  <-log(Stest$saldo_medio_var12_ult1) #:
      
      base
      num_var37_0	0.000925277	0.000745144
      FETrain$num_var37_0 <-Strain$num_var37_0
      FETest$num_var37_0  <-Stest$num_var37_0
      
      num_var41_0	0.000865841	0.000697279
      FETrain$num_var41_0 <-Strain$num_var41_0
      FETest$num_var41_0  <-Stest$num_var41_0
      
      
      log
      imp_aport_var13_hace3	0.00083061	0.000632469
      FETrain$imp_aport_var13_hace3 <-log(Strain$imp_aport_var13_hace3) #: 
      FETest$imp_aport_var13_hace3  <-log(Stest$imp_aport_var13_hace3) #:
      
      base
      num_op_var39_efect_ult3	0.000774303	0.000623562
      FETrain$num_op_var39_efect_ult3 <-Strain$num_op_var39_efect_ult3
      FETest$num_op_var39_efect_ult3  <-Stest$num_op_var39_efect_ult3
      
      num_meses_var13_corto_ult3	0.00069763	0.000561815
      num_meses_var13_corto_ult3 <-Strain$num_meses_var13_corto_ult3
      num_meses_var13_corto_ult3  <-Stest$num_meses_var13_corto_ult3
      
      ind_var26_cte	0.000655559	0.000527935
      FETrain$ind_var26_cte <-Strain$ind_var26_cte
      FETest$ind_var26_cte  <-Stest$ind_var26_cte
      
      var21	0.000643383	0.000518129
      FETrain$var21 <-Strain$var21
      FETest$var21  <-Stest$var21
      
      ind_var5_0	0.000621314	0.000500357
      FETrain$ind_var5_0 <-Strain$ind_var5_0
      FETest$ind_var5_0  <-Stest$ind_var5_0
      
      
      [48]	train-auc:0.906892+0.002341	test-auc:0.836464+0.012611
      
      
      ex
      saldo_medio_var12_hace3	0.000709907	0.000523789
      FETrain$saldo_medio_var12_hace3 <-exp(Strain$saldo_medio_var12_hace3)
      FETest$saldo_medio_var12_hace3  <-exp(Stest$saldo_medio_var12_hace3) 
      
      log
      saldo_var13	9.58E-05	      
      FETrain$saldo_var13 <-log(Strain$saldo_var13) #: 
      FETest$saldo_var13  <-log(Stest$saldo_var13) #:
      
      base
      num_var39_0	0.000580444	0.000467444
      FETrain$num_var39_0 <-Strain$num_var39_0
      FETest$num_var39_0  <-Stest$num_var39_0
      
      log
      saldo_var13_corto	0.00049648	
      FETrain$saldo_var13_corto <-log(Strain$saldo_var13_corto) #: 
      FETest$saldo_var13_corto  <-log(Stest$saldo_var13_corto) #:
      
      saldo_var14	0.000473354	
      FETrain$saldo_var14 <-log(Strain$saldo_var14) #: 
      FETest$saldo_var14  <-log(Stest$saldo_var14) #:
      
      base
      imp_var7_recib_ult1	0.000586867	0.000472616
      FETrain$imp_var7_recib_ult1 <-Strain$imp_var7_recib_ult1
      FETest$imp_var7_recib_ult1  <-Stest$imp_var7_recib_ult1
      
      saldo_medio_var12_ult3	0.000452928	0.000364752
      FETrain$saldo_medio_var12_ult3 <-Strain$saldo_medio_var12_ult3
      FETest$saldo_medio_var12_ult3  <-Stest$saldo_medio_var12_ult3
      
      num_op_var41_comer_ult3	0.000479626	0.000386253
      FETrain$num_op_var41_comer_ult3 <-Strain$num_op_var41_comer_ult3
      FETest$num_op_var41_comer_ult3  <-Stest$num_op_var41_comer_ult3
      
      ind_var1	0.000408355	0.000328856
      Strain$ind_var1 <-Strain$ind_var1
      FETest$ind_var1  <-Stest$ind_var1
      
      log
      saldo_medio_var13_corto_ult3	0.000289737
      FETrain$saldo_medio_var13_corto_ult3 <-log(Strain$saldo_medio_var13_corto_ult3) #: 
      FETest$saldo_medio_var13_corto_ult3  <-log(Stest$saldo_medio_var13_corto_ult3) #:
      
      saldo_var24	0.000278167
      FETrain$saldo_var24 <-log(Strain$saldo_var24) #: 
      FETest$saldo_var24  <-log(Stest$saldo_var24) #:
      
      saldo_var12	0.000255943
      FETrain$saldo_var12 <-log(Strain$saldo_var12) #: 
      FETest$saldo_var12  <-log(Stest$saldo_var12) #:
      
      #
      base
      num_op_var39_hace2	0.00036718	0.000295697
      FETrain$num_op_var39_hace2 <-Strain$num_op_var39_hace2
      FETest$num_op_var39_hace2  <-Stest$num_op_var39_hace2
      
      ind_var25_cte	0.000338234	0.000272386
      FETrain$ind_var25_cte <-Strain$ind_var25_cte
      FETest$ind_var25_cte  <-Stest$ind_var25_cte
      
      ind_var19	0.000335507	0.00027019
      FETrain$ind_var19 <-Strain$ind_var19
      FETest$nind_var19  <-Stest$ind_var19
      
      num_var5_0	0.000296356	0.000238662
      FETrain$num_var5_0 <-Strain$num_var5_0
      FETest$num_var5_0  <-Stest$num_var5_0
      
      num_op_var39_efect_ult1	0.000276671	0.000222808
      FETrain$num_op_var39_efect_ult1 <-Strain$num_op_var39_efect_ult1
      FETest$num_op_var39_efect_ult1  <-Stest$num_op_var39_efect_ult1
      
      num_op_var39_ult3	0.000240875	0.000193981
      FETrain$num_op_var39_ult3 <-Strain$num_op_var39_ult3
      FETest$num_op_var39_ult3  <-Stest$num_op_var39_ult3
      
      log
      saldo_var31	0.000127253
      FETrain$saldo_var31 <-log(Strain$ssaldo_var31) #: 
      FETest$saldo_var31  <-log(Stest$saldo_var31) #:
      
      
      base
      ind_var31_0	0.000173439	0.000139674
      FETrain$ind_var31_0 <-Strain$ind_var31_0
      FETest$ind_var31_0  <-Stest$nind_var31_0
      
      ind_var12_0	0.000167282	0.000134715
      FETrain$ind_var12_0 <-Strain$ind_var12_0
      FETest$ind_var12_0  <-Stest$ind_var12_0
      
      num_var5	0.000160965	0.000129629
      FETrain$num_var5 <-Strain$num_var5
      FETest$num_var5  <-Stest$num_var5
      