library('MDPtoolbox')

p0up = .55; p0down = .05; p0stay = 1-p0up-p0down
p1up = .45; p1down = .1; p1stay = 1-p1up-p1down
p2up = .2; p2down = .2; p2stay = 1-p2up-p2down
p3up = .05; p3down = .6; p3stay = 1-p3up-p3down
disc_factor = .9

P0 = matrix(rep(0, 36), 6, 6, byrow=TRUE); P1 = P0; P2 = P0; P3 = P0

P0[(row(P0) - col(P0)) == 1] <- p0down
P0[(row(P0) - col(P0)) == 0] <- p0stay
P0[(row(P0) - col(P0)) == -1] <- p0up
P0[1,1] = .8; P0[1,2] = 1-P0[1,1]; P0[6,6] = p0stay + p0up

P1[(row(P1) - col(P1)) == 1] <- p1down
P1[(row(P1) - col(P1)) == 0] <- p1stay
P1[(row(P1) - col(P1)) == -1] <- p1up
P1[1,1] = 1; P1[1,2] = 1-P1[1,1]; P1[6,6] = p1stay + p1up

P2[(row(P2) - col(P2)) == 1] <- p2down
P2[(row(P2) - col(P2)) == 0] <- p2stay
P2[(row(P2) - col(P2)) == -1] <- p2up
P2[1,1] = 1; P2[1,2] = 1-P2[1,1]; P2[6,6] = p2stay + p2up

P3[(row(P3) - col(P3)) == 1] <- p3down
P3[(row(P3) - col(P3)) == 0] <- p3stay
P3[(row(P3) - col(P3)) == -1] <- p3up
P3[1,1] = 1; P3[1,2] = 1-P3[1,1]; P3[6,6] = p3stay + p3up

P <- array(0, c(6,6,4))
P[,,1] <- P0; P[,,2] <- P1; P[,,3] <- P2; P[,,4] <- P3

R = matrix(rep(0:5, 4), 6, 4) 
R[,1] = 0*R[,1]; R[,3] = 2*R[,3]; R[,4] = 3*R[,4]

output = mdp_value_iteration(P, R, discount=disc_factor, epsilon = 10^-13)
print(output$V)
print(output$policy - 1)


p = function(s, a, sp){
  return(P[s+1, sp+1, a+1])
}

r = function(s, a){
  s*a
}
