#' RC-class linreg
#'
#' @field dep numeric.
#' @field indep matrix.
#' @field betahat numeric.
#' @field predicted matrix.
#' @field residuals matrix.
#' @field inv_sqr_dep matrix.
#' @field deg_fre numeric.
#' @field sigmasq numeric.
#' @field std_e numeric.
#' @field var_betas numeric.
#' @field se_betas numeric.
#' @field t_values numeric.
#' @field p_values numeric.
#' @field data data.frame.
#' @field formula formula.
#' @field dataname character.
#' @field output data.frame.
#'
#' @return
#' @export linreg
#' @exportClass linreg
#' @import ggplot2
#'
linreg <- setRefClass("linreg",
                      fields = list(dep = "numeric", indep = "matrix", betahat = "numeric",
                                    predicted = "matrix", residuals = "matrix",inv_sqr_dep = "matrix",
                                    deg_fre = "numeric", sigmasq="numeric", std_e= "numeric" , var_betas = "numeric",
                                    se_betas = "numeric" ,t_values = "numeric",p_values = "numeric" ,data="data.frame",
                                    formula="formula", dataname="character", output="data.frame"),
                      methods = list(
                        coef = function(){
                          "Method coef"

                          sqr_dep<-t(indep)%*%indep #creates the square matrix

                          inv_sqr_dep<<-solve(sqr_dep) # Creates an inverse matrix to the square matrix

                          t_indep_dep<-t(indep)%*%dep #creates a matrix that is solvebal

                          betahat1 <- inv_sqr_dep%*%t_indep_dep # Creates betahat for the regression

                          betahat<<-as.vector(betahat1[,1])

                        },
                        pred = function(){
                          "Method for prediction"
                          predicted <<- indep%*%betahat
                        },
                        resid = function(){residuals<<- dep - predicted },
                        dfre = function(){ deg_fre<<- nrow(indep) - ncol(indep)  },
                        sigma= function(){   sig <- (t(residuals)%*%residuals)/deg_fre
                        sig <- sig
                        std_e<<- as.numeric(sqrt(sig))
                        sigmasq<<- as.numeric(sig)
                        },
                        vbetas=function(){  var <-diag(sigmasq*inv_sqr_dep)
                        se_betas  <<-  sqrt(var)
                        var_betas <<-  var  # Sums the variance for each colum.
                        },
                        tvalue=function(){
                          "method t-value"
                          t_values <<- as.numeric( betahat/ se_betas )
                          p_values <<- pt(t_values, deg_fre)},
                        plot  = function(){
                          "plots the residuals versus fitted"
                          df<-data.frame(predicted, residuals)
                          ggplot(data = df, aes(x =predicted, y=residuals))  },
                        summary=function(){

                          df<-data.frame(betahat, se_betas, t_values, p_values)
                          rownames(df)<-colnames(indep)
                          namn<-colnames(indep)
                          #df<-as.table(df)
                          #base::print(df)


                          base::print(sprintf("%s %1.2f %1.2f %1.2f . ***",colnames(indep), betahat,se_betas, t_values))
                          #base::print(sprintf("%s %f1.1 %f1.1 %f1.1 . ***",colnames(indep), linreg_mod$betahat,linreg_mod$se_betas, linreg_mod$t_values ))
                          base::print(sprintf("Residual standard error: %1.1f on %g degrees of freedom", std_e, deg_fre))



                        },
                        print=function(){
                          form<-format(formula)
#                          form3<-format(formula[3])



                          base::print(paste0("linreg","(formula = ",form, ", data = ", dataname,")\n\n"))
                          base::print(paste0("(Intercept )" ,form))


                        },
                        initialize = function(form, data){
                          "Formula for reggression"

                          dataname<<-deparse(substitute(data))
                          data<<-data
                          indep<<-model.matrix(form,data)
                          dep<<-data[,all.vars(form)[1]]



                          formula<<-form

                          coef();pred();resid();dfre();sigma();vbetas();tvalue()






                        }

                      ) )

