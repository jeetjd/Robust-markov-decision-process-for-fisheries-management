rm(list = ls()); gc()

library(lpSolve); library(ggplot2)

source('nonrobust_fish_MDP_v2.R')

c = seq(.2, 1, by = .1)
tau = seq(.2, 1, by = .1)
c_tau = expand.grid(c,tau)
value_its = 100
all_V = matrix(0, nrow = nrow(c_tau), ncol = 6)
all_pol = matrix(0, nrow = nrow(c_tau), ncol = 6)

solve_my_lp = function(Pnom, V, c, tau){
  
  if(length(Pnom) == 3){
    
    f.obj = V
    f.con = matrix (c(1, 1, 1,
                      1, 0, 0,
                      0, 1, 0,
                      0, 0, 1,
                      1, 0, 0,
                      0, 1, 0,
                      0, 0, 1,
                      1, 0, 0,
                      0, 1, 0,
                      0, 0, 1,
                      1, 1, 1,
                      -1, 1, 1,
                      1, -1, 1,
                      1, 1, -1,
                      -1, -1, 1,
                      -1, 1, -1,
                      1, -1, -1,
                      -1, -1, -1), ncol=3, byrow=TRUE)
    f.dir = c("=", ">=", ">=", ">=",
              "<=", "<=", "<=", ">=", ">=", ">=",
              "<=", "<=", "<=", "<=", "<=", "<=", "<=", "<=")
    f.rhs = c(1, 0, 0, 0, 
              Pnom[1]+tau, Pnom[2]+tau, Pnom[3]+tau, 
              Pnom[1]-tau, Pnom[2]-tau, Pnom[3]-tau,
              c*tau + Pnom[1] + Pnom[2] + Pnom[3],
              c*tau - Pnom[1] + Pnom[2] + Pnom[3],
              c*tau + Pnom[1] - Pnom[2] + Pnom[3],
              c*tau + Pnom[1] + Pnom[2] - Pnom[3],
              c*tau - Pnom[1] - Pnom[2] + Pnom[3],
              c*tau - Pnom[1] + Pnom[2] - Pnom[3],
              c*tau + Pnom[1] - Pnom[2] - Pnom[3],
              c*tau - Pnom[1] - Pnom[2] - Pnom[3])
    
    return(lp("min", f.obj, f.con, f.dir, f.rhs)$solution)
    
  }else if(length(Pnom)==2){
    
    f.obj = V
    f.con = matrix (c(1, 1,
                      1, 0,
                      0, 1,
                      1, 0,
                      0, 1,
                      1, 0,
                      0, 1,
                      1, 1,
                      -1, 1,
                      1, -1,
                      -1, -1), ncol=2, byrow=TRUE)
    f.dir = c("=", ">=", ">=", 
              "<=", "<=", ">=", ">=", 
              "<=", "<=", "<=", "<=")
    f.rhs = c(1, 0, 0,  
              Pnom[1]+tau, Pnom[2]+tau, 
              Pnom[1]-tau, Pnom[2]-tau, 
              c*tau + Pnom[1] + Pnom[2],
              c*tau - Pnom[1] + Pnom[2],
              c*tau + Pnom[1] - Pnom[2],
              c*tau - Pnom[1] - Pnom[2])
    
    return(lp("min", f.obj, f.con, f.dir, f.rhs)$solution)
    
  }
  
}

for(j in 1:nrow(c_tau)){
  
  c = c_tau$Var1[j]
  tau = c_tau$Var2[j]
    
for(i in 1:value_its){
  
  if(i == 1){
    V_old = rep(0, 6) #starting value function
    pol = rep(0, 6)
  }else{
    V_old = V_new
  }
  
  V_new = rep(0, 6)
  
  for(s in 0:5){
    if(s == 0){
      V = V_old[1:2]
    }else if(s == 5){
      V = V_old[5:6]
    }else{
      V = V_old[s:(s+2)]
    }
    
    Q = rep(0, 4)
    
    for(a in 0:3){
      
      if(s == 0){
        Pnom = c(p(0, a, 0), p(0, a, 1))
      }else if(s == 5){
        Pnom = c(p(5, a, 4), p(5, a, 5))
      }else{
        Pnom = c(p(s, a, s-1), p(s, a, s), p(s,a,s+1))
      }
      Q[a+1] = r(s,a) + disc_factor*sum(solve_my_lp(Pnom, V, c, tau)*V)
    }
    
    V_new[s+1] = max(Q) #maximize over actions
    pol[s+1] = which.max(Q) - 1
  }
  
}

#print(V_new)
#print(pol)

all_V[j, ] = V_new
all_pol[j, ] = pol

if(j%%5==0){
  print(paste0(j, '/', nrow(c_tau)))
}


}


plot_stuff = cbind(c_tau, all_V[, 6])
names(plot_stuff) = c('c', 'tau', 'V5')
plot_stuff$rel_V5 = plot_stuff$V5/output$V[6]

plot_stuff$pol_match_nom = 0
for(j in 1:nrow(all_pol)){
  if(all((output$policy-1)==all_pol[j,])){
    plot_stuff$pol_match_nom[j] = 1
  }
}

ggplot()+
  geom_tile(data = plot_stuff, aes(x = c, y = tau, fill = rel_V5))+
  scale_fill_viridis_c()+
  #geom_contour(data = plot_stuff, aes(x = c, y = tau, z = pol_match_nom),
  #             bins = 1, color = 'black')
  labs(fill = 'Value ratio:\nrobust vs\nnonrobust')

print('done!')
