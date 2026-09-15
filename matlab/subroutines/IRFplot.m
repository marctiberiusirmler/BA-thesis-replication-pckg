function IRFplot(irfStruct, cholSet, H, shockName)
    nVars    = numel(cholSet);
    nHorizon = H;
    plotCols = 3;
    plotRows = ceil(nVars/plotCols);

    lineColor      = [0 0.4 0.8];
    bandFillLight  = [.85 .85 .85];
    bandFillDark   = [.7  .7  .7];

    figure('Name',['IRFs to a ' shockName ' Shock'],'NumberTitle','off','Color','w');
    for j = 1:nVars
        subplot(plotRows, plotCols, j); hold on;
        upper90 = squeeze(irfStruct.irfs_u(j,:,2));
        lower90 = squeeze(irfStruct.irfs_l(j,:,2));
        fill([0:nHorizon, fliplr(0:nHorizon)], [upper90, fliplr(lower90)], bandFillLight,'EdgeColor','none');
        upper68 = squeeze(irfStruct.irfs_u(j,:,1));
        lower68 = squeeze(irfStruct.irfs_l(j,:,1));
        fill([0:nHorizon, fliplr(0:nHorizon)], [upper68, fliplr(lower68)], bandFillDark,'EdgeColor','none');
        plot(0:nHorizon, zeros(1,nHorizon+1), 'k--');
        plot(0:nHorizon, irfStruct.irfs(j,:), 'LineWidth',1.5,'Color',lineColor);
        grid on; xlim([0 nHorizon]);
        set(gca,'XTick',0:6:nHorizon,'XTickLabel',cellstr(num2str((0:6:nHorizon)')));
        title(cholSet{j}, 'FontSize',9,'FontWeight','normal');
        if j==1, ylabel('Response (% points)','FontSize',9); end
        if j>=(plotRows-1)*plotCols+1, xlabel('Horizon'); end
        hold off;
    end
end