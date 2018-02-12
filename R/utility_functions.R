estimateVar_SI<-function(input_X,input_Y){
        sd=array(NA,10)
        for (m in 1:10){
                temp=estimateSigma(input_X,input_Y)$sigmahat
                sd[m]=ifelse(temp%in%c(Inf,NaN),NA,temp)
        }
        sigma_sq_est_SI=mean(sd,na.rm=T)^2
        return(sigma_sq_est_SI)
}

##-------------------- MSE, MSE, MSE! --------------------------##
get_mse<-function(estimation,true){
        return(mean((estimation-true)^2))
}

consistent<-function(true,estimate,tolerrance){
        bi_true=rep(0,p)
        bi_true[which(abs(true) > tolerrance)] = 1
        bi_est=rep(0,p)
        bi_est[which(abs(estimate) > tolerrance)] = 1
        tab=table(bi_true,bi_est)
        sen=tab[2,2]/sum(tab[2,])
        spec=tab[1,1]/sum(tab[1,])
        return(list(sensitivity=sen,specificity=spec))
}

relative_change<-function(x,x_ref){
        return((x-x_ref)/x_ref)
}

