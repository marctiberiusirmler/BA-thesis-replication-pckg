%copied from computeIRFs to determine optimal lag length in MAIN_SVAR.m 
%without error

function res = estimateVAR(data,nLags)
%estimateVAR  Estimate a VAR(p) with constant term
%   res.Y       = dependent matrix [T−p × n]
%   res.Ylag    = lagged regressors [T−p × (1 + n*p)]
%   res.beta    = (1+n*p)-by-n matrix of coefficients
%   res.resid   = VAR residuals [T−p × n]
%   res.data    = original data
%   res.nlags   = p

    [T,n] = size(data); 
    p     = nLags;

    % build matrix of lagged Y (no constant)
    Ylag = nan(T-p, n*p);
    for j = 1:p
        Ylag(:, n*(j-1)+1 : n*j) = data(p-j+1 : end-j, :);
    end

    % trim for effective sample
    Y       = data(p+1:end, :);
    nT      = size(Ylag,1);

    % estimate via OLS: [1, Ylag] * beta = Y
    X = [ones(nT,1), Ylag];
    beta = X \ Y;

    innovations = Y - X*beta;

    res.Y       = Y;
    res.Ylag    = Ylag;
    res.beta    = beta;
    res.resid   = innovations;
    res.data    = data;
    res.nlags   = p;
end