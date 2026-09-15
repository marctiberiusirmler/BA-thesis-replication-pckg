function IRFmatrixPDF(IRF_all, cholSet, H, filename, varargin)
% IRF_all : cell array. Each cell can be a struct with fields:
%           irfs [nVars x H+1], irfs_l/u [nVars x H+1 x 2], name
% cholSet : cellstr of variable names (rows/cols)
% H       : max horizon (integer)
% filename: output PDF
% varargin: 
%   'mode' 'image'|'vector' (default 'image' for robustness),
%   'dpi'  (default 300)
%   'lineColor'      (default [0 0.4 0.8])
%   'bandFillLight'  (default [.85 .85 .85])   % 90% band
%   'bandFillDark'   (default [.70 .70 .70])   % 68% band

    p = inputParser;
    addParameter(p,'mode','image');
    addParameter(p,'dpi',300);
    addParameter(p,'lineColor',[0 0.4 0.8]);      % <<< NEW
    addParameter(p,'bandFillLight',[.85 .85 .85]);% <<< NEW
    addParameter(p,'bandFillDark',[.70 .70 .70]); % <<< NEW
    parse(p,varargin{:});
    mode = validatestring(p.Results.mode, {'image','vector'});
    dpi  = p.Results.dpi;

    % pick colors
    lineColor     = p.Results.lineColor;
    bandFillLight = p.Results.bandFillLight;
    bandFillDark  = p.Results.bandFillDark;

    nVars = numel(cholSet);
    hvec  = 0:H;

    fig = figure('Name','IRF Matrix','NumberTitle','off', ...
                 'Units','pixels','Position',[100 100 1400 1000], ...
                 'Color','w','Visible','on','Renderer','opengl');

    set(fig, 'Color','w', 'InvertHardcopy','off', ...       
         'DefaultAxesColor','w', ...                  
         'DefaultAxesXColor','k','DefaultAxesYColor','k','DefaultAxesZColor','k');

    tiledlayout(nVars, nVars, 'TileSpacing','compact','Padding','compact');

    for c = 1:nVars
        shockLabel = upper(cholSet{c});
        S = coerceIRFCell(IRF_all, c, nVars, numel(hvec), shockLabel);

        for r = 1:nVars
            ax = nexttile((r-1)*nVars + c);
            set(ax, 'Color','w', 'XColor','k', 'YColor','k');
            hold(ax,'on');

            [ok90, yU90, yL90] = extractBand(S, r, 2);
            if ok90
                fill(ax, [hvec, fliplr(hvec)], [yU90, fliplr(yL90)], ...
                     bandFillLight, 'EdgeColor','none');
            end

            [ok68, yU68, yL68] = extractBand(S, r, 1);
            if ok68
                fill(ax, [hvec, fliplr(hvec)], [yU68, fliplr(yL68)], ...
                     bandFillDark, 'EdgeColor','none');
            end

            plot(ax, hvec, zeros(size(hvec)), 'k--');

            y = extractIRF(S, r);
            if any(isfinite(y))
                plot(ax, hvec, y, 'LineWidth',1.1, 'Color', lineColor);
            end

            xlim(ax,[0 H]); grid(ax,'on');
            set(ax,'XTick',0:6:H,'XTickLabel',[],'FontSize',7);

            if c == 1
                ylabel(ax, lower(cholSet{r}), 'Interpreter','none','FontSize',8);
            end
            if r == nVars
                set(ax,'XTickLabel',string(0:6:H));
                xlabel(ax,'Horizon','FontSize',8);
            end
            if r == 1
                tt = 'Shock: ';
                if isfield(S,'name') && ~isempty(S.name), tt = [tt S.name];
                else, tt = [tt shockLabel];
                end
                title(ax, tt, 'FontWeight','normal','FontSize',9);
            end

            hold(ax,'off');
        end
    end

    drawnow; pause(0.05);

    try
        if strcmp(mode,'image')
            exportgraphics(fig, filename, 'ContentType','image', ...
                           'Resolution', dpi, 'BackgroundColor','white');
        else
            exportgraphics(fig, filename, 'ContentType','vector', ...
                           'BackgroundColor','white');
        end
    catch ME
        warning('exportgraphics failed (%s). Falling back to print -dpdf.', ME.message);
        try
            set(fig,'PaperPositionMode','auto');
            print(fig, filename, '-dpdf', '-painters');
        catch ME2
            close(fig);
            rethrow(ME2);
        end
    end

    close(fig);
    fprintf('PDF saved: %s\n', filename);
end

% ---------------- helpers (unchanged from your robust version) ----------
function S = coerceIRFCell(IRF_all, idx, nVars, nH, shockLabel)
    Sname = shockLabel;
    S = struct('irfs', nan(nVars, nH), 'irfs_l', nan(nVars, nH, 2), ...
               'irfs_u', nan(nVars, nH, 2), 'name', Sname);

    if idx > numel(IRF_all) || isempty(IRF_all{idx}), return, end
    raw = IRF_all{idx};

    if isstruct(raw)
        if isfield(raw,'irfs') && ~isempty(raw.irfs),   S.irfs   = fitToDims(raw.irfs,   nVars, nH); end
        if isfield(raw,'irfs_l') && ~isempty(raw.irfs_l), S.irfs_l = fitBandToDims(raw.irfs_l, nVars, nH); end
        if isfield(raw,'irfs_u') && ~isempty(raw.irfs_u), S.irfs_u = fitBandToDims(raw.irfs_u, nVars, nH); end
        if isfield(raw,'name') && ~isempty(raw.name),   S.name   = char(string(raw.name)); end
        return
    end

    if isnumeric(raw)
        if isequal(size(raw), [nVars, nH])
            S.irfs = raw;
        elseif isequal(size(raw), [nH, nVars])
            S.irfs = raw.';
        else
            if size(raw,2)==nH && size(raw,1)<=nVars
                S.irfs(1:size(raw,1), :) = raw;
            elseif size(raw,1)==nH && size(raw,2)<=nVars
                S.irfs(1:size(raw,2), :) = raw.'; 
            end
        end
    end
end

function A = fitToDims(Ain, nVars, nH)
    if ndims(Ain) ~= 2, A = nan(nVars, nH); return, end
    sz = size(Ain);
    if isequal(sz, [nVars, nH]), A = Ain;
    elseif isequal(sz, [nH, nVars]), A = Ain.';
    else
        A = nan(nVars, nH);
        r = min(nVars, sz(1)); c = min(nH, sz(2));
        A(1:r, 1:c) = Ain(1:r, 1:c);
    end
end

function B = fitBandToDims(Bin, nVars, nH)
    if ndims(Bin) ~= 3, B = nan(nVars, nH, 2); return, end
    sz = size(Bin);
    if isequal(sz, [nVars, nH, 2]), B = Bin;
    elseif isequal(sz, [nH, nVars, 2]), B = permute(Bin, [2 1 3]);
    else
        B = nan(nVars, nH, 2);
        r = min(nVars, sz(1)); c = min(nH, sz(2)); t = min(2, sz(3));
        B(1:r, 1:c, 1:t) = Bin(1:r, 1:c, 1:t);
    end
end

function [ok, yU, yL] = extractBand(S, r, bandIdx)
    yU = squeeze(S.irfs_u(r,:,bandIdx)); yL = squeeze(S.irfs_l(r,:,bandIdx));
    ok = any(isfinite(yU)) && any(isfinite(yL)) && numel(yU)==numel(yL);
    if ~ok, yU = []; yL = []; end
end

function y = extractIRF(S, r)
    y = S.irfs(r,:); if isempty(y) || all(~isfinite(y)), y = nan(1, size(S.irfs,2)); end
end