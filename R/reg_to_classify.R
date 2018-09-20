#' High dimensional LDA to build classifyer using regression results
#'
#' \code{reg.to.classify} estimated beta coefficients and pseudo probablity
#' @param x predictor matrix of dimension \eqn{n*q} used for previous regression 
#' @param y continuous outcome vector of dimension \eqn{n} used for previous regression
#' @param beta_hat estimated beta coefficients from regression (no intercept term)
#' @param x_predict data for prediction (no column of 1 needed)
#' @return Returns the estimated beta coefficients for classification and pseudo probablity
#' @examples
#' reg.to.classify(train_X,train_Y,cusLasso.result$coef,testX)

reg.to.classify<-function(x,y,beta_hat,x_predict){
                mu_1_hat_vec = apply(x[which(y==1),], 2, mean)
                mu_2_hat_vec = apply(x[which(y==2),], 2, mean)
                
                beta1<-beta_hat*as.numeric(sign(t(mu_2_hat_vec-mu_1_hat_vec)%*%beta_hat))
                #beta0<--t((mu_2_hat_vec + mu_1_hat_vec))%*%beta1/2-log(n_1/n_2)*diag(t(beta1)%*%covariance_hat%*%beta1)/(t(mu_2_hat_vec-mu_1_hat_vec)%*%beta1)
                beta0 <- 0
                beta <- c(beta0,beta_hat)
                
                pred<-as.matrix(cbind(rep(1,nrow(x_predict)),x_predict)%*%beta)
                #pred<-ifelse(pred>0,2,1)
                return(list(beta.classify = beta,pred = pred))
        }
