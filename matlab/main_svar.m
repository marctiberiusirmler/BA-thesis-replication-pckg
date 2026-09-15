% main_svar.m
%
% Adapted from code and data published by Silvia Miranda-Agrippino:
% https://silviamirandaagrippino.com/code-data
%
% Estimates the "adjusted" 7-variable Cholesky-identified SVAR (GDP, GDPDEF,
% CREDIT, INFLOWS, BDLEV, WXSR, VIX) for the full sample and for pre-/post-
% 2009Q2 subsamples, selects VAR lag order by AIC/BIC, computes impulse
% responses to a WXSR shock and a VIX shock with wild-bootstrap error bands,
% and exports the IRF CSVs. Produces Figures 8, 9, D.1-D.4.
%
% Requires: MATLAB with the Statistics and Machine Learning Toolbox (for
% basic distribution functions used in the bootstrap; no other toolboxes
% required
%
% Run from anywhere: this script locates its own folder via mfilename and
% cd's there


clear
clc

thisFile = mfilename('fullpath');
cd(fileparts(thisFile));
addpath('subroutines');

%read data ----------------------------------------------------------------
%T = readtable('../data/build/svar/svar_data_rey_spec.csv');  %Rey data
T = readtable('../data/build/svar/svar_data_adjusted_spec.csv'); %updated data
data     = table2array( T(:, 1:end) );
varnames = lower( T.Properties.VariableNames(1:end) );
dataStructure.data        = data;
dataStructure.varname     = varnames;
dataStructure.varLongName = varnames;

outDir = '../output/figures';
if ~exist(outDir, 'dir'), mkdir(outDir); end

% -------------------------------------------------------------------------
% FULL SAMPLE
% -------------------------------------------------------------------------

%determine optimal lag length ---------------------------------------------
maxLags = 12;
[nObs,n] = size(data);
AIC = nan(maxLags,1);
BIC = nan(maxLags,1);

for p = 1:maxLags
    VARp = estimateVAR(data,p);
    Sigma = cov(VARp.resid);
    T_eff = size(VARp.Y,1);
    k = n + n^2 * p;
    logL = -T_eff*n/2*(log(2*pi)+1) - T_eff/2*log(det(Sigma));
    AIC(p) = -2*logL + 2*k;
    BIC(p) = -2*logL + log(T_eff)*k;
end

[~,optAIC] = min(AIC);
[~,optBIC] = min(BIC);
%modelSpec.nLags = 2;  %Rey spec
modelSpec.nLags = optBIC;  %VAR lags, based on BIC
fprintf('Selected lags by BIC = %d  (AIC suggests %d)\n', optBIC, optAIC);

%model specification ------------------------------------------------------
modelSpec.nHorizons = 20; %max horizon for IRFs
modelSpec.cLevel =[68 90]; %size of error bands
modelSpec.bootSize =1000;  %size of bootstrap sample for error bands

%-1-CHOLESKY full sample---------------------------------------------------

modelSpec.identification        ='CHOL';

%select relevant data
%cholSet                        ={'gdp';'gdpdef';'credit';'inflows';'eulev';'ffr';'vix'}; %Rey (7-var, original spec)
cholSet                          ={'gdp';'gdpdef';'credit';'inflows';'bdlev';'wxsr';'vix'}; %adjusted (7-var)
[~,dataSelection]               =ismember(cholSet,dataStructure.varname);
modelSpec.dataSelection         =dataSelection;

%MP shock
%shockVar                        ='ffr'; %Rey
shockVar                        ='wxsr';
modelSpec.shockVar              =ismember(cholSet,shockVar);
%modelSpec.shockSize             =0.25*double(ismember(cholSet,shockVar)); %Rey
modelSpec.shockSize             =1*double(ismember(cholSet,shockVar));
irfWXSR = computeIRFs(dataStructure, modelSpec); %compute response

%VIX shock
shockVar = 'vix';
modelSpec.shockVar  = ismember(cholSet, shockVar);
modelSpec.shockSize = log(1.01) * double(modelSpec.shockVar);
irfVIX = computeIRFs(dataStructure, modelSpec); %compute response

%export IRFs --------------------------------------------------------------
H = modelSpec.nHorizons;
horiz = (0:H)';

%plot IRF to check --------------------------------------------------------
IRFplot(irfVIX,  cholSet, H, 'VIX');
IRFplot(irfWXSR, cholSet, H, 'WXSR');

% WXSR shock export (FULL, Figure 8)
writematrix([horiz, irfWXSR.irfs'],               [outDir '/figure08_irf_wxsr_shock_full_sample.csv']);
writematrix([horiz, irfWXSR.irfs_l(:,:,1)'],      [outDir '/figure08_irf_wxsr_shock_full_sample_68_lower.csv']);
writematrix([horiz, irfWXSR.irfs_u(:,:,1)'],      [outDir '/figure08_irf_wxsr_shock_full_sample_68_upper.csv']);
writematrix([horiz, irfWXSR.irfs_l(:,:,2)'],      [outDir '/figure08_irf_wxsr_shock_full_sample_90_lower.csv']);
writematrix([horiz, irfWXSR.irfs_u(:,:,2)'],      [outDir '/figure08_irf_wxsr_shock_full_sample_90_upper.csv']);

% VIX shock export (FULL, Figure 8)
writematrix([horiz, irfVIX.irfs'],                [outDir '/figure08_irf_vix_shock_full_sample.csv']);
writematrix([horiz, irfVIX.irfs_l(:,:,1)'],       [outDir '/figure08_irf_vix_shock_full_sample_68_lower.csv']);
writematrix([horiz, irfVIX.irfs_u(:,:,1)'],       [outDir '/figure08_irf_vix_shock_full_sample_68_upper.csv']);
writematrix([horiz, irfVIX.irfs_l(:,:,2)'],       [outDir '/figure08_irf_vix_shock_full_sample_90_lower.csv']);
writematrix([horiz, irfVIX.irfs_u(:,:,2)'],       [outDir '/figure08_irf_vix_shock_full_sample_90_upper.csv']);

% Full 7x7 IRF matrix (all shocks x all responses) for the appendix
% (Figure D.2, full sample). Commented out by default: each call
% re-estimates the full bootstrap (modelSpec.bootSize draws) once per shock
% (7x)
%{
IRF_all_Full = buildIRFAll(dataStructure, modelSpec, cholSet);
IRFmatrixPDF(IRF_all_Full, cholSet, modelSpec.nHorizons, ...
    [outDir '/figureD2_full_irf_grid_full_sample.pdf'], ...
    'mode','image','dpi',300, ...
    'lineColor',[0 0 0], ...
    'bandFillLight',[.85 .85 .85], ...
    'bandFillDark',[.70 .70 .70]);
%}

% -------------------------------------------------------------------------
% SUBSAMPLES
% -------------------------------------------------------------------------

% sample split ------------------------------------------------------------
startYear    = 1990;
startQuarter = 2;

rowFromYQ = @(Y,Q) (Y - startYear)*4 + (Q - startQuarter) + 1;

row_pre_start = 1;                            % 1990Q2
row_pre_end   = rowFromYQ(2009,2);            % 2009Q2

preIdx      = row_pre_start : row_pre_end;
postIdx     = (row_pre_end + 1) : nObs;       % 2009Q3-end

fprintf('PreGFC rows:  %d-%d (%d obs)\n', preIdx(1),  preIdx(end),  numel(preIdx));
fprintf('PostGFC rows: %d-%d (%d obs)\n', postIdx(1), postIdx(end), numel(postIdx));

% Subsample IRFs WXSR & VIX ------------------------------------------------
maxLags_sub = 12;
H           = modelSpec.nHorizons;
horiz       = (0:H)';

subsets = {
    'preGFC',    preIdx;
    'postGFC',   postIdx;
};

shockList = {
    'wxsr', 1;
    'vix', log(1.01)
};

IRF_sub   = struct();
lagReport = cell(size(subsets,1),3);
lagInfo   = struct();

for sset = 1:size(subsets,1)
    subName = subsets{sset,1};
    idx     = subsets{sset,2};

    dsSub          = dataStructure;
    dsSub.data     = dataStructure.data(idx,:);

    % get lags (subsample-specific)
    [optAIC_sub, optBIC_sub] = selectVARlags(dsSub.data(:, modelSpec.dataSelection), maxLags_sub);
    modelSpecSub       = modelSpec;
    modelSpecSub.nLags = optBIC_sub;

    lagReport{sset,1} = subName;
    lagReport{sset,2} = optAIC_sub;
    lagReport{sset,3} = optBIC_sub;
    lagInfo.(subName).optAIC = optAIC_sub;
    lagInfo.(subName).optBIC = optBIC_sub;

    for k = 1:size(shockList,1)
        shockVar               = shockList{k,1};
        shockSize              = shockList{k,2};
        modelSpecSub.shockVar  = ismember(cholSet, shockVar);
        modelSpecSub.shockSize = shockSize * double(modelSpecSub.shockVar);

        irfOut = computeIRFs(dsSub, modelSpecSub);

        % Figure 9: selected IRFs, split samples
        base = sprintf('%s/figure09_irf_%s_shock_%s', outDir, lower(shockVar), subName);
        writematrix([horiz, irfOut.irfs'],               [base '.csv']);
        writematrix([horiz, irfOut.irfs_l(:,:,1)'],      [base '_68_lower.csv']);
        writematrix([horiz, irfOut.irfs_u(:,:,1)'],      [base '_68_upper.csv']);
        writematrix([horiz, irfOut.irfs_l(:,:,2)'],      [base '_90_lower.csv']);
        writematrix([horiz, irfOut.irfs_u(:,:,2)'],      [base '_90_upper.csv']);

        IRF_sub.(subName).(upper(shockVar)) = irfOut;
    end
end

lagTbl = cell2table(lagReport, 'VariableNames', {'Subset','optAIC','optBIC'});
disp(lagTbl)

%plot IRF to check --------------------------------------------------------
for s = ["preGFC", "postGFC"]
    if isfield(IRF_sub.(s), 'WXSR'), IRFplot(IRF_sub.(s).WXSR, cholSet, H, ['WXSR - ' char(s)]); end
    if isfield(IRF_sub.(s), 'VIX'),  IRFplot(IRF_sub.(s).VIX,  cholSet, H, ['VIX - '  char(s)]); end
end

% 7x7 IRF matrix PDFs for preGFC (Figure D.3) and postGFC (Figure D.4).
% (Figure D.1, the Rey (2013)-specification grid, is not produced here)
% --- preGFC ---
%{
dsPre              = dataStructure;
dsPre.data         = dataStructure.data(preIdx,:);
[~, optBIC_pre]    = selectVARlags(dsPre.data(:, modelSpec.dataSelection), 12);
modelSpecPre       = modelSpec;
modelSpecPre.nLags = optBIC_pre;

IRF_all_Pre = buildIRFAll(dsPre, modelSpecPre, cholSet);

blue4_line = [0, 0, 139/255];
blue4_90   = [0.80, 0.85, 1.00];
blue4_68   = [0.65, 0.75, 1.00];

IRFmatrixPDF(IRF_all_Pre, cholSet, modelSpec.nHorizons, ...
    [outDir '/figureD3_full_irf_grid_preGFC.pdf'], ...
    'mode','image','dpi',300, ...
    'lineColor',blue4_line, ...
    'bandFillLight',blue4_90, ...
    'bandFillDark',blue4_68);
%}
% --- postGFC ---
%{
dsPost              = dataStructure;
dsPost.data         = dataStructure.data(postIdx,:);
[~, optBIC_post]    = selectVARlags(dsPost.data(:, modelSpec.dataSelection), 12);
modelSpecPost       = modelSpec;
modelSpecPost.nLags = optBIC_post;

IRF_all_Post = buildIRFAll(dsPost, modelSpecPost, cholSet);

darkorange_line = [1.00, 140/255, 0];
darkorange_90   = [1.00, 0.85, 0.60];
darkorange_68   = [1.00, 0.75, 0.40];

IRFmatrixPDF(IRF_all_Post, cholSet, modelSpec.nHorizons, ...
    [outDir '/figureD4_full_irf_grid_postGFC.pdf'], ...
    'mode','image','dpi',300, ...
    'lineColor',darkorange_line, ...
    'bandFillLight',darkorange_90, ...
    'bandFillDark',darkorange_68);
%}

%% ========================= LOCAL FUNCTIONS ==============================
function [optAIC,optBIC] = selectVARlags(y, maxLags)
    [~,n] = size(y);
    AIC = nan(maxLags,1); BIC = nan(maxLags,1);
    for p = 1:maxLags
        VARp  = estimateVAR(y, p);
        Sigma = cov(VARp.resid);
        T_eff = size(VARp.Y,1);
        k     = n + n^2 * p;
        logL  = -T_eff*n/2*(log(2*pi)+1) - T_eff/2*log(det(Sigma));
        AIC(p)= -2*logL + 2*k;
        BIC(p)= -2*logL + log(T_eff)*k;
    end
    [~,optAIC] = min(AIC);
    [~,optBIC] = min(BIC);
end

function IRF_all = buildIRFAll(ds, ms, cholSet)
    IRF_all = cell(numel(cholSet),1);
    for c = 1:numel(cholSet)
        shockVar     = cholSet{c};
        ms.shockVar  = ismember(cholSet, shockVar);
        ms.shockSize = shockSizeFor(shockVar) * double(ms.shockVar);
        irf          = computeIRFs(ds, ms);
        irf.name     = upper(shockVar);
        IRF_all{c}   = irf;
    end
end

function s = shockSizeFor(v)
    switch lower(v)
        case 'ffr', s = 0.25; %25 bp in percent units
        case 'wxsr', s = 1.00;       %100 bp in percent units
        case 'vix',  s = log(1.01);  %+1% in log terms
        otherwise,   s = 1;          %unit shock
    end
end
