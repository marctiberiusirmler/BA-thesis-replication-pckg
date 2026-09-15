function cumStruct = cumulativeIRFs(irfStruct, toCum, isLogDiff)
% makeCumulativeIRFs  Cumulate IRFs of differenced variables (and, if desired,
%                     turn cumulated log-diffs into % level changes).
%
% Inputs
%   irfStruct : struct with fields irfs (nVar x H+1), irfs_l, irfs_u, and
%               optionally irfs_boot (nVar x H+1 x B)
%   toCum     : logical vector (nVar x 1), 1 for variables that were differenced
%   isLogDiff : logical vector (nVar x 1), 1 for variables that were 100*Δlog
%
% Output
%   cumStruct : same fields as irfStruct but cumulated (and transformed)

    cumStruct = irfStruct;  % start by copying

    % -------- Point estimates --------
    cumStruct.irfs(toCum,:) = cumsum(irfStruct.irfs(toCum,:), 2);

    % Turn cumulated 100*Δlog into % level change: 100*(exp(sumΔlog/100)-1)
    idxLog = isLogDiff;
    if any(idxLog)
        L = cumStruct.irfs(idxLog,:) / 100;
        cumStruct.irfs(idxLog,:) = 100*(exp(L) - 1);
    end

    % -------- Error bands --------
    if isfield(irfStruct, 'irfs_boot')
        % Best: cumulate each draw, then re-compute quantiles
        boot = irfStruct.irfs_boot;                % nVar x H+1 x B
        boot(toCum,:,:) = cumsum(boot(toCum,:,:),2);

        if any(idxLog)
            for b = 1:size(boot,3)
                Lb = boot(idxLog,:,b) / 100;
                boot(idxLog,:,b) = 100*(exp(Lb) - 1);
            end
        end

        probs = [0.16 0.84; 0.05 0.95];            % 68% & 90%
        for ib = 1:2
            cumStruct.irfs_l(:,:,ib) = prctile(boot, probs(ib,1)*100, 3);
            cumStruct.irfs_u(:,:,ib) = prctile(boot, probs(ib,2)*100, 3);
        end
    else
        % Fallback: cumulate the published bands (approximation)
        cumStruct.irfs_l(toCum,:,:) = cumsum(irfStruct.irfs_l(toCum,:,:),2);
        cumStruct.irfs_u(toCum,:,:) = cumsum(irfStruct.irfs_u(toCum,:,:),2);

        if any(idxLog)
            for ib = 1:size(irfStruct.irfs_l,3)
                Llo = cumStruct.irfs_l(idxLog,:,ib) / 100;
                Lhi = cumStruct.irfs_u(idxLog,:,ib) / 100;
                cumStruct.irfs_l(idxLog,:,ib) = 100*(exp(Llo) - 1);
                cumStruct.irfs_u(idxLog,:,ib) = 100*(exp(Lhi) - 1);
            end
        end
    end
end