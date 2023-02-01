#STOCK-FLOW CONSISTENT MODEL

rm(list =ls(all=TRUE))
#time periods & scenarios
T<-51
Scenarios<-3

#################################
#Symbols of endogenous variables
#################################
Symbols<-c("W","Y_C","CO","D","Y","TP","RP","DP","I","K","L","BP","D_red",
           "Y_star","u","g_Y","lev")

##########
#Equations
##########
Equations<-function(t){
  #Households
  W[t]<<-s_W*Y[t] #Eq. (1)
  Y_C[t]<<-DP[i]+BP[t]+int_D*D[t-1] #Eq. (2)
  CO[t]<<-c_1*W[t-1]+c_2*Y_C[t-1]+c_3*D[t-1] #Eq. (3)
  D[t]<<-D[t-1]+W[t]+Y_C[t]-CO[t] #Eq. (4)
  #Firms
  Y[t]<<-CO[t]+I[t] #Eq. (5)
  TP[t]<<-Y[t]-W[t]-int_L*L[t-1] #Eq. (6)
  RP[t]<<-s_F*TP[t-1] #Eq. (7)
  DP[t]<<-TP[t]-RP[t] #Eq. (8)
  I[t]<<-g_K*K[t-1] #Eq. (9)
  K[t]<<-K[t-1]+I[t] #Eq. (10)
  L[t]<<-L[t-1]+I[t]-RP[t] #Eq. (11)
  #Banks
  BP[t]<<-int_L*L[t-1]-int_D*D[t-1] #Eq. (12)
  D_red[t]<<-L[t] #Eq. (13)
  #Auxiliary equations
  Y_star[t]<<-v*K[t] #Eq. (14)
  u[t]<<-Y[t]/Y_star[t] #Eq. (15)
  g_Y[t]<<-(Y[t]-Y[t-1])/Y[t-1] #Eq. (16)
  lev[t]<<-L[t]/K[t] #Eq. (17)
}
##############################
#Parameters and initial values
#############################
Parameters_InitialValues<-function(){
  #Parameters
  g_K<<-0.029
  c_1<<- 0.9
  c_2<<- 0.75
  int_D<<-0.025 
  int_L<<-0.08
  s_W<<-0.54 
  c_3<<-(CO[1]*(1+g_K)-c_1*W[1]-c_2*Y_C[1])/D[1]#based on Eq. (3)
  s_F<<-(I[1]*(1+g_K)-g_K*L[1])/TP[1]#based on Eq. (11)
  v<<-Y_star[1]/K[1]#based on Eq. (14)  
  #Initial values
  Y[1]<<-85.93
  I[1]<<-0.24*Y[1]
  L[1]<<-0.914*Y[1] 
  u[1]<<-0.72 
  W[1]<<-s_W*Y[1]#based on Eq. (1)
  Y_C[1]<<-DP[1]+BP[1]+int_D*(D[1]/(1+g_K))#based on Eq (2)
  CO[1]<<-Y[1]-I[1] #based on Eq. (5)
  TP[1]<<-Y[1]-W[1]-int_L*(L[1]/(1+g_K)) #based on Eq. (6)
  RP[1]<<-s_F*(TP[1]/(1+g_K)) #based on Eq. (7)
  DP[1]<<-TP[1]-RP[1] #based on Eq. (8)
  K[1]<<-(I[1]*(1+g_K))/g_K #based on Eq. (9) and Eq. (10)
  BP[1]<<-int_L*(L[1]/(1+g_K))-int_D*(D[1]/(1+g_K)) #based on Eq. (12) 
  D_red[1]<<-L[1] #based on Eq. (13) 
  D[1]<<-L[1] #based on Eq. (13) 
  Y_star[1]<<-Y[1]/u[1] #based on Eq. (15)
  g_Y[1]<<-g_K #based on Eq. (16)
  lev[1]<<-L[1]/K[1] #based on Eq. (17)
}

#########################
#Parameters for scenarios
#########################
Parameters_scenario2<-function(){
  s_W<<-0.54
}

Parameters_scenario3<-function(){
  int_L<<-0.08
}

##################################
#Vectors for endogenous variables
##################################
for (v in 1:length(Symbols)){assign(noquote(paste(Symbols[v])), vector(length=T))}

##############
#Run the model
###############
for (j in 1:Scenarios){
  for (iterations in 1:10){Parameters_InitialValues()}
  
  for (i in 2:T){
    if (j==2 & i>=20) {Parameters_scenario2()}
    if (j==3 & i>=20) {Parameters_scenario3()}
    
    for (iterations in 1:10){Equations(t=i)}
  }
  #################
  #Save the results
  #################
  matrixname<-paste("Table",j, sep="")
  values<-setNames(data.frame(matrix(ncol = length(Symbols), nrow = T)), Symbols)
  for (v in 1:length(Symbols)){values[,v]<
