### This file contains functions needed for our method when there is no external info
### provides an alternative to cross-validation
estimateVar_SI<-function(input_X,input_Y){
        sd=array(NA,10)
        for (m in 1:10){
                temp=estimateSigma(input_X,input_Y)$sigmahat
                sd[m]=ifelse(temp%in%c(Inf,NaN),NA,temp)
        }
        sigma_sq_est_SI=mean(sd,na.rm=T)^2
        return(sigma_sq_est_SI)
}

##------------ EB, direct optimizing the loglikelihood ----------##
EB_opt<-function(input_X,input_Y,maxstep,initial){
        X=input_X
        Y=input_Y
        gamma=initial[1]
        sigma2=initial[2]
        n=nrow(input_X)
        p=ncol(input_X)

        gamma_sample=matrix(NA,ncol = 1,nrow = maxstep)
        sigma2_sample=matrix(NA,ncol = 1,nrow = maxstep)
        k=1
        while(k<maxstep){

                big_sigma=ginv((1/sigma2)*t(X)%*%X+diag(rep(gamma,p)))
                big_mu=(1/sigma2)*big_sigma%*%t(X)%*%Y

                if (k>3){
                        distance=sum(gamma_sample[k-1]-gamma_sample[k-2])
                        if (distance<0.01){
                                break
                        }
                }

                eta=p-gamma*sum(diag(big_sigma))
                gamma=eta/(t(big_mu)%*%big_mu)
                yminus=Y-X%*%big_mu
                sigma2=as.numeric(t(yminus)%*%yminus/(n-eta))

                gamma_sample[k]=gamma
                sigma2_sample[k]=sigma2
                k=k+1
        }
        return(list(tau_est=sqrt(2*gamma),sigma2_est=sigma2))

}

EBestiamtes<-function(input_X,input_Y){
        # estimate sigma square from SI
        var_SI=estimateVar_SI(input_X,input_Y)
        # estiamtes from EB
        output_EB=EB_opt(input_X,input_Y,100,c(0.1,var_SI))
        estimated_tau=output_EB$tau_est
        var_EB=output_EB$sigma2_est
        estimated_variance=ifelse(var_SI<var_EB+10,var_SI,var_EB)
        # return parameters
        return(list(tauHat=estimated_tau,VarianceHat=estimated_variance,tuningParameter=estimated_tau*estimated_variance/(nrow(input_X))))
}

# Estimates(train_X,train_y)


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
