# X;Y;Z;sigma.square = estimateVariance(X,Y);
# alpha.init = rep(1,ncol(Z));
# delta.init = rep(0,ncol(X));
# maxstep = 100;
# margin = 0.001;
# maxstep_inner = 100;
# tol.delta = 0.001
CusLasso.l2 <- function(X,Y,Z,sigma.square = estimateVariance(X,Y),
                        alpha.init = rep(1,ncol(Z)),
                        delta.init = rep(1,ncol(X)),
                        maxstep = 100,
                        margin = 0.001,
                        maxstep_inner = 100,
                        tol.delta = 0.001){
        n = nrow(X);p=ncol(X);q = ncol(Z)
        ## Initialize
        alpha.old = alpha.init
        k = 1
        while(k < maxstep){
                gamma = 2*exp(-2*Z%*%alpha.old)
                Sigma_y = sigma.square * diag(n) + X %*% diag(c(gamma)) %*% t(X)
                theta = diag(t(X) %*% solve(Sigma_y,X))
                
                ## update alpha given theta
                alpha.new <- update_alpha(delta.init = delta.init,theta=theta,maxstep_inner = maxstep_inner,tol.delta = tol.delta)$alpha.est
                
                if(sum(abs(alpha.new - alpha.old)) < margin ){
                        break
                }
                alpha.old <- alpha.new
                k <- k+1
                print(k)
        }
        coef.ard = diag(c(gamma))%*%t(X)%*%solve(Sigma_y,Y)

        tauEst = exp(Z%*%alpha.old)
        pen_vec= tauEst*sigma.square/n
        C= sum(pen_vec)/p
        coef.cusLasso = coef(glmnet(X,Y,alpha = 1, lambda = C, penalty.factor = pen_vec))
        
        return(list(coef.ard = coef.ard,coef.cusLasso = coef.cusLasso,alpha.hat = alpha.old,n_iter = k,sigma.square = sigma.square))
}
