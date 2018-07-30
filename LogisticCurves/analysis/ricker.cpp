#include <TMB.hpp>

template<class Type>
bool isNA(Type x) {
    return R_IsNA(asDouble(x));
}

template<class Type>
Type objective_function<Type>::operator() (){
    
    DATA_VECTOR(log_y);
    DATA_IVECTOR(iyear);
    DATA_IVECTOR(isurvey);
    DATA_INTEGER(nyears);
    DATA_INTEGER(n);
    
    PARAMETER(log_K); Type K = exp(log_K); 
    PARAMETER(log_r); Type r = exp(log_r);
    PARAMETER_VECTOR(log_sigma_y); vector<Type> sigma_y = exp(log_sigma_y);
    PARAMETER_VECTOR(log_q); vector<Type> q = exp(log_q);
    PARAMETER_VECTOR(log_x); vector<Type> x = exp(log_x);
    PARAMETER(log_sigma_x); Type sigma_x = exp(log_sigma_x);
    
    Type nll = 0;
    
    vector<Type> pred_x(nyears);
    for (int i = 1; i < nyears; i++) {
        pred_x(i) = log_x(i - 1) + r * (1 - (exp(log_x(i - 1)) / K));
        nll -= dnorm(log_x(i), pred_x(i), sigma_x, true);
        SIMULATE{
            log_x(i) = rnorm(pred_x(i), sigma_x);
        }
    }
    vector<Type> pred_y(n);
    for(int i = 0; i < n; i++) {
        pred_y(i) = log_q(isurvey(i)) + log_x(iyear(i));
        if(!isNA(log_y(i))) {
            nll -= dnorm(log_y(i), pred_y(i), sigma_y(isurvey(i)), true);
            SIMULATE{
                log_y(i) = rnorm(pred_y(i), sigma_y(isurvey(i)));
            }
        }
    }
    
    SIMULATE{
        REPORT(log_x);
        REPORT(log_y);
    }
    ADREPORT(pred_x);
    ADREPORT(pred_y);
    
    return nll;
    
}
