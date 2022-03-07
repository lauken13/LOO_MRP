## Simulation study for LOO_MRP
## as in Little (2003)
## 20/09/2021

N = 10000

sim_data = as.data.frame(matrix(NA, nrow=N, ncol=5))
colnames(sim_data)=c('Z', 'X', 'Y', 'S', 'R')

## generating Z and X ####
set.seed(123456)
ZX_gen = rmultinom(n=N, size=1, prob=matrix(c(0.3,0.4,0.2,0.1),nrow=2))

ZX_tab = matrix(c(apply(ZX_gen,1,sum)),nrow=2)
rownames(ZX_tab) = c("X=0", "X=1")
colnames(ZX_tab) = c("Z=0", "Z=1")
ZX_tab

ZX_gen2 = as.data.frame(t(ZX_gen))

# to fill in the values of Z and X's
for (i in 1:N){
  if(ZX_gen2[i,'V1'] == 1){
    sim_data[i,'Z'] = 0
    sim_data[i,'X'] = 0
  }
  
  if(ZX_gen2[i,'V2'] == 1){
    sim_data[i,'Z'] = 0
    sim_data[i,'X'] = 1
  }
  
  if(ZX_gen2[i,'V3'] == 1){
    sim_data[i,'Z'] = 1
    sim_data[i,'X'] = 0
  }
  
  if(ZX_gen2[i,'V4'] == 1){
    sim_data[i,'Z'] = 1
    sim_data[i,'X'] = 1
  }
}

sum(sim_data$X == 0) # should match sum(ZX_tab[1,])
sum(sim_data$X == 1) # should match sum(ZX_tab[2,])
sum(sim_data$Z == 0) # should match sum(ZX_tab[,1])
sum(sim_data$Z == 1) # should match sum(ZX_tab[,2])


## generating Y's -- five scenarios ####
gam_x = c(2,2,2,0,0)
gam_z = c(2,2,0,2,0)
gam_xz = c(2,0,0,0,0)

x_mean = mean(sim_data$X)
z_mean = mean(sim_data$Z)

# empty matrix to fill in Y's for diff scenarios
Y_mat = matrix(NA, nrow=N, ncol=length(gam_x))

# scenario 1 - 5
for(s in 1:length(gam_x)){
  Y_mat[,s] = 0.5 + gam_x[s]*(sim_data$X - x_mean) + gam_z[s]*(sim_data$Z - z_mean) +
    gam_xz[s]*(sim_data$X - x_mean)*(sim_data$Z - z_mean)
}
inv_logit_scaled(Y_mat[,1])

Y_invlogit = apply(Y_mat, 2, inv_logit_scaled)

Y_outcome = apply(Y_invlogit, 2, function(x)rbinom(N,1,x))

## distribution of S given Z, X, and Y ####
sum(sim_data$Z == 1)

## distribution of R given Z, X, Y and S ####
beta_x = gam_x
beta_z = gam_z
beta_xz = gam_xz

R_mat = matrix(NA, nrow=N, ncol=length(beta_x))

for(s in 1:length(beta_x)){
  R_mat[,s] = 0.5 + beta_x[s]*(sim_data$X - x_mean) + beta_z[s]*(sim_data$Z - z_mean) +
    beta_xz[s]*(sim_data$X - x_mean)*(sim_data$Z - z_mean)
}

R_invlogit = apply(R_mat, 2, inv_logit_scaled)

R_outcome = apply(R_invlogit, 2, function(x)rbinom(N,1,x))


## fill in the 5 x 5 combination of data in a list
empty_list = lapply(1:25, function(x)matrix(c(sim_data$Z, sim_data$X, rep(NA,N), rep(NA,N), rep(NA,N)), nrow=N, ncol=5))
sim_data_list = lapply(empty_list, function(x){colnames(x)=c('Z', 'X', 'Y', 'S', 'R');x})

str(sim_data_list)

## fill in Y values in each matrix
Y_vec = rep(1:5, times=5)
for(i in 1:25){
    sim_data_list[[i]][,3] = Y_outcome[,Y_vec[i]]
}

## fill in R values in each matrix
R_vec = rep(1:5, each=5)
for(i in 1:25){
    sim_data_list[[i]][,5] = R_outcome[,R_vec[i]]
}



