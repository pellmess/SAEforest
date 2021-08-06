# THESE ARE ORIGINAL AND UNCHANGED SUPPORTFUNCTIONS FROM
# THE METHOD OF LI and LiU 2019


##
fan1 <- function(y, lam){

  # f <- c()
  #
  # for(i in 1:length(y)){
  #   if(y[i] >= 0){
  #     f[i]  <- y[i]^(1/lam)
  #   }else{
  #     f[i] <- sign(y[i])*abs(y[i])^(1/lam)
  #   }
  # }

  f <- rep(NA, length(y))

  index_tmp     <- which(y >= 0)
  f[ index_tmp] <- y[index_tmp]^(1/lam)
  f[-index_tmp] <- sign(y[-index_tmp])*abs(y[-index_tmp])^(1/lam)

  return(f)
}


##
ptf1 <- function(y,lam ){
  return(sign(y)*abs(y)^lam)
}

##
## This is the profile log-likelihood function of parameter gama.
## gama :: variance ratio

plf <- function(gama, x, z, m, num, p){

  A  <- list()

  for(j in 1:m){
    aa     <- gama/(1 + gama*num)
    A[[j]] <- diag(num) - aa * matrix(1, num, num)
  }

  #MLE for beta
  a = 0
  b = matrix(0, p, p)
  c = rep(0, p)

  for(k in 1:m){
    ind        = (a + 1):(a + num)
    beta_part1 = t(x[ind,]) %*% A[[k]] %*% x[ind,]
    beta_part2 = t(x[ind,]) %*% A[[k]] %*% z[ind]
    b = b + beta_part1
    c = c + beta_part2
    a = a + num
  }

  tmp = ginv(b)
  beta_mle_orgin = tmp %*% c

  # MLE for sigma
  M   = 0
  SSE = rep(0, m)
  d   = 0

  for(l in 1:m){
    inde     = (d + 1):(d + num)
    err_res  = z[inde] - x[inde,] %*% beta_mle_orgin
    SSE[l]   = t(err_res) %*% A[[l]] %*% err_res
    d        = d + num
  }

  NK= rep(num, m)
  M = num * m *log(sum(SSE)/ (m* num)) + sum(log(1 + gama * NK))
  return(M)
}

plf_gama <- function(gama, z){
  return(plf(gama, x = x, z = z, m = m, num = num, p = p))
}

##
## This is the  log-likelihood function of parameter lambda.
llf1 <- function(lam, x, y, m, num, p){

  z        <- ptf1(y,lam)

  gama_hat <- optimize(plf_gama, lower = 0.001, upper = 100, maximum = F, z = z)$minimum
  A        <- list()
  xishu    <- rep(0, m)
  for(s in 1:m){
    xishu[s]<- gama_hat/(1 + gama_hat * num)
    A[[s]]  <- diag(num) - xishu[s] * matrix(1, nrow = num, ncol = num)
  }
  a = 0
  b = matrix(0, p, p)
  c = rep(0, p)
  for(k in 1:m){
    ind = (a + 1):(a + num)
    beta_part1 = t(x[ind,]) %*% A[[k]] %*% x[ind,]
    beta_part2 = t(x[ind,]) %*% A[[k]] %*% z[ind]
    b = b + beta_part1
    c = c + beta_part2
    a = a + num
  }
  part = ginv(b)
  beta_mle = part %*% c
  SSE = rep(0, m)
  d = 0
  for(v in 1:m){
    inde = (d + 1):(d + num)
    err_res = z[inde] - x[inde,] %*% beta_mle
    SSE[v]  = t(err_res) %*% A[[v]] %*% err_res
    d = d + num
  }
  sig2_hat = sum(SSE)/(num*m)
  NK= rep(num, m)
  Q = (-1/2) * (num*m) * log(sig2_hat) + sum(log(1 + gama_hat*NK))+ # Achtung
    (lam-1) * sum(log(abs(y))) + (num*m) * log(lam)
  Q = -Q

  return(Q)
}

llf1_lam <- function(lam){return(llf1(lam, x = x, y = y, m = m, num = num, p = p))}
##
## This is the profile log-likelihood function of parameter gama (untransformed).
## gama :: variance ratio


plfu <- function(gama, x, y, m, num, p){

  A  <- list()
  for(j in 1:m){
    aa     = gama/(1 + gama * num)
    A[[j]] = diag(num) - aa* matrix(1, num, num)
  }

  ##  MLE for beta
  a = 0
  b = matrix(0, p, p)
  c = rep(0, p)
  for(k in 1:m){
    ind = (a + 1):(a + num)
    beta_part1 = t(x[ind,]) %*% A[[k]] %*% x[ind,]
    beta_part2 = t(x[ind,]) %*% A[[k]] %*% y[ind]
    b = b + beta_part1
    c = c + beta_part2
    a = a + num
  }
  tmp = ginv(b)
  beta_mle_orgin = tmp %*% c

  ## MLE for sigma
  M   = 0
  SSE = rep(0, m)
  d   = 0
  for(l in 1:m){
    inde     = (d + 1):(d + num)
    err_res  = y[inde] - x[inde,] %*% beta_mle_orgin
    SSE[l]   = t(err_res) %*% A[[l]] %*% err_res
    d        = d + num
  }

  NK= rep(num, m)
  M = (num*m) * log(sum(SSE)/(num*m)) + sum(log(1 + gama*NK)) # Veraenderung !!
  return(M)
}

plfu_gama <- function(gama){
  return(plfu(gama, x = x, y = y, m = m, num = num, p = p))
}



####
elm <- function(x, mu = rep(0,ncol(x)), lam = t(rep(0,ncol(x))), maxit = 25,
                gradtol = 1e-7, svdtol = 1e-9, itertrace = 0){

  #  ELM        computes the empirical likelihood for the mean

  #  elm(x, mu, lam, maxit, gradtol, svdtol, itertrace )

  #  Input      Meaning                            Default

  #  x          n x p matrix of data
  #  mu         hypothesized mean value            zeros(1,p)
  #  lam        guess for Lagrange multiplier      zeros(1,p)
  #  maxit      maximum iterations                 25
  #  gradtol    solve grad=0 to this tolerance     1e-7
  #  svdtol     tolerance in SVD                   1e-9
  #  itertrace  1 to trace iterations              0
  #
  #  Output     Meaning                            Default
  #
  #  logelr     log empirical likelihood
  #  lambda     Lagrange multiplier
  #  grad       gradient vector
  #  hess       Hessian matrix
  #  wts        output weights
  #  nits       number of iterations

  # Check Input

  n = nrow(x)
  p = ncol(x)

  if(ncol(x) != length(mu)){stop('Need size(mu) = size(x(i,:)')}

  if(length(lam) != length(mu)){stop('Lam must be sized like mu')}

  if(gradtol <  1e-16){gradtol = 1e-16}
  if(svdtol  <  1e-8){gradtol   = 1e-8}

  z = x - t(matrix(mu, nrow = p, ncol= n))

  # Step weights: inner search starts with
  # Newton step, then if necessary gets smaller
  # and more parallel to the gradient

  newton_wts     =   c(1/3 ^c(0:3), rep(0,12))
  gradient_wts   =   .5 ^c(0:15)
  gradient_wts   =   (gradient_wts^2 - newton_wts^2)^0.5

  gradient_wts[12:16] = gradient_wts[12:16] / (.1^-(1:5))


  #  Outer loop of iteration.  When all goes well
  # this is simply a Newton iteration.

  nits  = 0
  gsize = gradtol + 1.0

  while(nits < maxit & gsize > gradtol){

    arg  = 1 + z %*% lam
    wts1 = plog(arg ,1/n,1 )
    wts2 = (- plog( arg,1/n,2 ))^0.5

    grad = -z * matrix(wts1, nrow = nrow(wts1), ncol = p)
    grad = colSums(grad)
    gsize= mean(abs(grad))

    hess = z * matrix(wts2, nrow = nrow(wts2), ncol = p) # matrix sqrt of hessian

    hu  = svd(hess)$u
    hs  = diag(length(svd(hess)$d)) * svd(hess)$d
    hv  = svd(hess)$v
    dhs = svd(hess)$d

    if(min(dhs) < max(dhs)*svdtol + 1e-128){
      dhs = dhs + svdtol*max(dhs) + 1e-128
    }

    dhs= 1/dhs
    hs = diag(dhs)

    nstep = hv %*% hs %*% t(hu) %*% (wts1/wts2)
    gstep = -grad
    if(sum(nstep^2) < sum(gstep^2)){
      gstep = gstep*sum(nstep^2)^0.5/sum(gstep^2)^0.5
    }

    ologelr = -sum(plog(arg, 1/n))
    ninner  = 0

    for(i in 1:length(newton_wts)){
      nlam    = lam + newton_wts[i] * nstep + gradient_wts[i] * gstep
      nlogelr = -sum(plog(1 + z %*% nlam, 1/n))

      if(nlogelr < ologelr){
        lam    = nlam
        ninner = i
        break
      }
    }

    nits = nits+1
    if(ninner == 0){
      nits = maxit
    }

    if(itertrace == 1){
      print(list(lam,nlogelr,gsize,ninner,nits))
    }
  }

  logelr = nlogelr
  lambda = lam
  hess   = t(hess) %*% hess
  wts    = wts1

  Ergebnisse <- list(logelr, lambda, grad, hess, wts, nits)
  names(Ergebnisse) <- c("logelr", "lambda", "grad", "hess", "wts", "nits")

  return(Ergebnisse)
}



####
plog <- function(z,eps,d = 0){

  ## Pseudo logarithm

  #    The pseudo logarithm agrees with log for arguments larger
  #   than eps.  Below eps it is a quadratic.  It has two continuous
  #   derivatives.

  #    Input

  #    z    =  matrix of pseudolog arguments
  #    eps  =  threshold
  #    d    =  0,1,2, for function, 1st, 2nd deriv


  zsize = dim(z)
  out   = c(z)
  low   = out < eps

  if(d == 0){
    out[!low] = log(out[!low])
    out[ low] = log(eps) - 1.5 + 2*out[low]/eps - 0.5*(out[low]/eps)^2
  }else if(d == 1){
    out[!low] = 1/out[!low]
    out[ low] = 2/eps - out[low]/eps^2
  }else if(d == 2){
    out[!low] = -1/out[!low]^2
    out[ low] = -1/eps^2
  }else{
    stop('Unknown option d for pseudologarithm')
  }
  llogz = matrix(out, zsize[1], zsize[2])
  return(llogz)
}

TNER1_schleife <- function(x_jj_beta_alpha){

  tmp2 = fan1(x_jj_beta_alpha + rnorm(10000, mean = 0, sd = sqrt(sig2_hat)), lam = lam_hat)

  return(mean(tmp2))
}

# _____________________________________________OWN__________________________________

elm_wrapper <- function(X_input_elm,mu_input_elm){
  n = nrow(X_input_elm)
  p = ncol(X_input_elm)

  elm_results <- elm(x = X_input_elm , mu =  mu_input_elm, lam = rep(0, p), maxit = 25, gradtol = 1e-7,
                     svdtol = 1e-9, itertrace = 0)

  logelr = elm_results$logelr
  lambda = elm_results$lambda

  zz    = X_input_elm - t(matrix(mu_input_elm, nrow = p, ncol = n))
  tmp3   = n * (zz %*% lambda + 1)
  # Gewicht mit Nebenbedingungen bestimmen
  prob   = 1/tmp3

  return(list(prob = prob, experWeight = elm_results$wts))
}
