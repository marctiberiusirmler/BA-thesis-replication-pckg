function out = FEVD_WXSR_on_VIX(ds, ms, cholSet)
% Compute FEVD share of VIX explained by WXSR over horizons 0..H.
% Uses computeIRFs; forces a minimal bootstrap size to avoid indexing issues.

    H = ms.nHorizons;
    n = numel(cholSet);
    iVIX  = find(strcmpi(cholSet,'vix'), 1);
    jWXSR = find(strcmpi(cholSet,'wxsr'),1);
    assert(~isempty(iVIX) && ~isempty(jWXSR), 'FEVD needs vix and wxsr in cholSet.');

    % Build MA coeffs for unit structural shocks
    Theta = zeros(n,n,H+1);
    msLoc = ms;

    % --- Important: avoid bootSize=0 so computeSirfBands never indexes at 0
    % Use a small, safe number of draws. 10 is the minimum so that round(0.05*nB)>=1.
    msLoc.bootSize = max(10, 10);
    % Bands are irrelevant for FEVD, but set cLevel anyway.
    if ~isfield(msLoc,'cLevel') || isempty(msLoc.cLevel)
        msLoc.cLevel = [68 90];
    end

    for j = 1:n
        sel = false(n,1); sel(j) = true;   % column logical n×1
        ssz = zeros(n,1); ssz(j) = 1;      % unit shock, column n×1

        msLoc.shockVar  = sel;
        msLoc.shockSize = ssz;

        irf = computeIRFs(ds, msLoc);      % expects column vectors
        Theta(:,j,:) = irf.irfs;
    end

    % FEVD(VIX <- WXSR)
    num = cumsum( squeeze(Theta(iVIX,jWXSR,:)).^2 );
    den = zeros(H+1,1);
    for j = 1:n
        den = den + cumsum( squeeze(Theta(iVIX,j,:)).^2 );
    end
    share = num ./ den;

    out.h = (0:H)'; out.share = share; out.Theta = Theta;

    % Print nicely
    showH = unique([0 1 4 8 12 20 H]); showH = showH(showH<=H);
    fprintf('FEVD VIX <- WXSR (share, horizons):\n');
    for hh = showH(:).'
        fprintf('  h=%2d : %.2f%%\n', hh, 100*share(hh+1));
    end
end