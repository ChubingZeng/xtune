#' @title Estimate the variance of a linear regression
#' @description This estimates the variance of a linear regression by SI
#' @param Input_X Predictors
#' @param Input_Y Outcome
#' @seealso \code{\link[glmnet]{cv.glmnet}}
#' @export
#' @example \dontrun{
#'  # I don't want you to run this
#' }
#' x = matrix(rnorm(1000),ncol=10)
#' y = rnorm(100)
#' estimateVar_SI(x,y)
#'

##------------------- Esitmating error variance ------------------##
checkargs.xy <- function(x, y) {
        if (missing(x)) stop("x is missing")
        if (is.null(x) || !is.matrix(x)) stop("x must be a matrix")
        if (missing(y)) stop("y is missing")
        if (is.null(y) || !is.numeric(y)) stop("y must be numeric")
        if (ncol(x) == 0) stop("There must be at least one predictor [must have ncol(x) > 0]")
        if (checkcols(x)) stop("x cannot have duplicate columns")
        if (length(y) == 0) stop("There must be at least one data point [must have length(y) > 0]")
        if (length(y)!=nrow(x)) stop("Dimensions don't match [length(y) != nrow(x)]")
}
checkcols <- function(A) {
        b = rnorm(nrow(A))
        a = sort(t(A)%*%b)
        if (any(diff(a)==0)) return(TRUE)
        return(FALSE)
}

estimateSigma <- function(x, y, intercept=TRUE, standardize=TRUE) {
        checkargs.xy(x,rep(0,nrow(x)))
        if(nrow(x)<10) stop("Number of observations must be at least 10 to run estimateSigma")
        cvfit=cv.glmnet(x,y,intercept=intercept,standardize=standardize)
        lamhat=cvfit$lambda.min
        fit=glmnet(x,y,standardize=standardize)
        yhat=predict(fit,x,s=lamhat)
        nz=sum(predict(fit,s=lamhat, type="coef")!=0)
        sigma=sqrt(sum((y-yhat)^2)/(length(y)-nz-1))
        return(list(sigmahat=sigma, df=nz))
}

estimateVar_SI<-function(input_X,input_Y){
        sd=array(NA,10)
        for (m in 1:10){
                temp=estimateSigma(input_X,input_Y)$sigmahat
                sd[m]=ifelse(temp%in%c(Inf,NaN),NA,temp)
        }
        sigma_sq_est_SI=mean(sd,na.rm=T)^2
        return(sigma_sq_est_SI)
}

EB_single<-function(input_X,input_Y,maxstep,initial){
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
                sigma2=initial[2]
                #sigma2=initial[2]

                gamma_sample[k]=gamma
                sigma2_sample[k]=sigma2
                k=k+1
        }
        return(list(tau_est=sqrt(2*gamma),sigma2_est=sigma2))

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
                #sigma2=initial[2]

                gamma_sample[k]=gamma
                sigma2_sample[k]=sigma2
                k=k+1
        }
        return(list(tau_est=sqrt(2*gamma),sigma2_est=sigma2))

}

Estimates<-function(input_X,input_Y){
        # estimate sigma square from SI
        var_SI=estimateVar_SI(input_X,input_Y)
        # estiamtes from EB
        output_EB=EB_opt(input_X,input_Y,100,c(0.1,var_SI))
        estimated_tau=output_EB$tau_est
        var_EB=output_EB$sigma2_est
        # compare which one gives a lower training mse
        # coef_eb_varSI=estimate_beta(input_X[1:(nrow(input_X)*0.7),],input_Y[1:(nrow(input_X)*0.7)],estimated_tau,var_SI)
        # coef_eb_varEB=estimate_beta(input_X[1:(nrow(input_X)*0.7),],input_Y[1:(nrow(input_X)*0.7)],estimated_tau,var_EB)
        # train_mse_SI=get_mse(cbind(rep(1,nrow(input_X)),input_X)%*%coef_eb_varSI,input_Y)
        # train_mse_EB=get_mse(cbind(rep(1,nrow(input_X)),input_X)%*%coef_eb_varEB,input_Y)
        # output estimated variance
        estimated_variance=ifelse(var_SI<var_EB+10,var_SI,var_EB)
        # return parameters
        return(list(tauHat=estimated_tau,VarianceHat=estimated_variance,tuningParameter=estimated_tau*estimated_variance/(nrow(input_X))))
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

estimate_beta<-function(input_X,input_y,tau_est,sigma_sq_est){
        n=nrow(input_X)
        return(coef(glmnet(input_X, input_y, alpha = 1, lambda = tau_est*sigma_sq_est/n)))
}
