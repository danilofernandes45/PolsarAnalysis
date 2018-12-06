library(pracma)
library(ppls)

kennaugh_vector <- array(0, dim = c(nrow, ncol, 16))

kennaugh_vector[,,1] <- (hhhh+hvhv+vvvv) / 2
kennaugh_vector[,,2] <- (hhhh+hvhv-vvvv) / 2
kennaugh_vector[,,3] <- (hhhh-hvhv+vvvv) / 2
kennaugh_vector[,,4] <- (-hhhh+hvhv+vvvv) / 2

# kennaugh_vector[,,5] <- Re(hhhv)
# kennaugh_vector[,,6] <- Re(hhvv)
# kennaugh_vector[,,7] <- Im(hvvv)
# 
kennaugh_vector[,,8] <- Re(hvvv)
kennaugh_vector[,,9] <- Im(hhvv)
kennaugh_vector[,,10] <- -Im(hhhv)

kennaugh_vector[,,11] <- Re(hhhv)
kennaugh_vector[,,12] <- Re(hhvv)
kennaugh_vector[,,13] <- Im(hvvv)

kennaugh_vector[,,14] <- Re(hvvv)
kennaugh_vector[,,15] <- Im(hhvv)
kennaugh_vector[,,16] <- -Im(hhhv)

# kennaugh_vector[,,5] <- Re_hhhv
# kennaugh_vector[,,6] <- Re_hhvv
# kennaugh_vector[,,7] <- Im_hvvv
# 
# kennaugh_vector[,,8] <- Re_hvvv
# kennaugh_vector[,,9] <- Im_hhvv
# kennaugh_vector[,,10] <- -Im_hhhv
# 
# kennaugh_vector[,,11] <- Re_hhhv
# kennaugh_vector[,,12] <- Re_hhvv
# kennaugh_vector[,,13] <- Im_hvvv
# 
# kennaugh_vector[,,14] <- Re_hvvv
# kennaugh_vector[,,15] <- Im_hhvv
# kennaugh_vector[,,16] <- -Im_hhhv

id1 <- sample(1:nrow, size = 10000, replace = TRUE)
id2 <- sample(1:ncol, size = 10000, replace = TRUE)

norm_kennaugh <- array(0, dim = 10000)

#ppls + Norm (pracma)
for (i in 1:10000) {
  versor <- normalize.vector(kennaugh_vector[id1[i], id2[i],])
  norm_kennaugh[i] <- Norm( versor )
}
#Mode   FALSE    TRUE    NA's 
#logical    3259    6705      36 

#2*eps
#   Mode   FALSE    TRUE    NA's 
#logical     110    9854      36 

#ppls  + norm (base)
for (i in 1:10000) {
  versor <- normalize.vector(kennaugh_vector[id1[i], id2[i],])
  norm_kennaugh[i] <- norm( as.matrix( versor ), type = "F")
}
#Mode   FALSE    TRUE    NA's 
#logical    4775    5189      36

#2*eps
#   Mode   FALSE    TRUE    NA's 
#logical     681    9283      36 

#bio3d + Norm
for (i in 1:10000) {
  versor <- normalize.vector(as.matrix(kennaugh_vector[id1[i], id2[i],]))
  norm_kennaugh[i] <- Norm(  versor )
}
#   Mode   FALSE    TRUE    NA's 
#logical    3259    6705      36 

#2*eps
#   Mode   FALSE    TRUE    NA's 
#logical     110    9854      36 

#bio3d + norm
for (i in 1:10000) {
  versor <- normalize.vector(as.matrix(kennaugh_vector[id1[i], id2[i],]))
  norm_kennaugh[i] <- norm( as.matrix( versor ), type = "F")
}
#   Mode   FALSE    TRUE    NA's 
#logical    4775    5189      36 

#2*eps
#   Mode   FALSE    TRUE    NA's 
#logical     681    9283      36 

#Norm
for (i in 1:10000) {
  versor <- kennaugh_vector[id1[i], id2[i],] / Norm(kennaugh_vector[id1[i], id2[i],])
  norm_kennaugh[i] <- Norm( versor )
}

#  Mode   FALSE    TRUE    NA's 
#logical    3259    6705      36

#2*eps
#   Mode   FALSE    TRUE    NA's 
#logical     110    9854      36 

#norm
for (i in 1:10000) {
  versor <- kennaugh_vector[id1[i], id2[i],] / norm(as.matrix(kennaugh_vector[id1[i], id2[i],]), type = "F")
  norm_kennaugh[i] <- norm( as.matrix( versor ), type = "F")
}
#    Mode   FALSE    TRUE    NA's 
#logical    3395    6569      36 

#2*eps
#   Mode   FALSE    TRUE    NA's 
#logical     183    9781      36 

comparation <- norm_kennaugh == 1
summary(comparation)

sum(norm_kennaugh, na.rm = TRUE)/10000 == 1 #false

comparation <- (1 - .Machine$double.neg.eps*2) < norm_kennaugh & norm_kennaugh < (1 + .Machine$double.eps*2)
summary(comparation)
