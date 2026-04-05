clear; clc;

%% Directory & Load Data
basePath = 'D:\Forest_loss\HPC\Plots\before_vs_after\';
eiFiles = {'ei1.csv', 'ei2.csv', 'ei5.csv'};
rbFiles = {'rb1.csv', 'rb2.csv', 'rb5.csv'};
years   = [1, 2, 5];

%% Color palette
colors = [
    249/255, 166/255, 2/255;    % 1 yellow
    59/255, 177/255, 67/255;    % 2 green (EI)
    58/255, 67/255, 186/255;    % 3 blue (RB)
];

%% Setup Figure
fig = figure('Units', 'inches', 'Position', [1, 1, 12, 6], 'Color', 'w');
t = tiledlayout(2, 3, 'TileSpacing', 'compact', 'Padding', 'loose');

panelLabels = {'(a)','(b)','(c)','(d)','(e)','(f)'};
xOffset = 0.01; yOffset = 0.98;

%% Plotting
for i = 1:3
    
    %% ===== RB (Row 1) =====
    dataRB = readtable(fullfile(basePath, rbFiles{i}));
    
    before = dataRB.rb_before1;
    after  = dataRB.rb_after1;
    delta  = after - before;

    nexttile(i);

    plot_delta_regression(before, delta, ...
        sprintf('RB %d', years(i)), colors(3,:), ...
        'ΔRB (After - Before)', 'RB Before');

    xlim([0 1.5]);
    xticks([0 0.5 1 1.5]);

    text(xOffset, yOffset, panelLabels{i}, 'Units','normalized', ...
        'FontWeight','bold','FontSize',17,'FontName','Arial', ...
        'VerticalAlignment','top','HorizontalAlignment','left');

    if i ~= 1
        set(gca, 'YTickLabel', []);
        ylabel('');
    end

    %% ===== EI (Row 2) =====
    dataEI = readtable(fullfile(basePath, eiFiles{i}));
    
    before = dataEI.ei_before1;
    after  = dataEI.ei_after1;
    delta  = after - before;

    nexttile(i + 3);

    plot_delta_regression(before, delta, ...
        sprintf('EI %d', years(i)), colors(2,:), ...
        'ΔEI (After - Before)', 'EI Before');

    xlim([0 1]);
    xticks([0 0.2 0.4 0.6 0.8 1]);

    text(xOffset, yOffset, panelLabels{i+3}, 'Units','normalized', ...
        'FontWeight','bold','FontSize',17,'FontName','Arial', ...
        'VerticalAlignment','top','HorizontalAlignment','left');

    if i ~= 1
        set(gca,'YTickLabel',[]);
        ylabel('');
    end
end

%% Shared X labels
axTop = nexttile(2);
xlabel(axTop, 'RB Before', 'FontName','Times New Roman','FontSize',15);

axBottom = nexttile(5);
xlabel(axBottom, 'EI Before', 'FontName','Times New Roman','FontSize',15);

%% ===== Export Figure =====
set(fig, 'Color', 'w');
exportgraphics(fig, 'forest_loss_plots.png', ...
               'Resolution', 300, ...
               'BackgroundColor', 'white'); 

disp('Figure exported to forest_loss_plots.png');

%% ===== Plot Function =====
function plot_delta_regression(x, y, labelPrefix, ptColor, ylab, xlab)

    % Remove NaNs
    validIdx = ~isnan(x) & ~isnan(y);
    x = x(validIdx);
    y = y(validIdx);

    hold on;

    %% Regression line for visualization
    coeffs = polyfit(x, y, 1);          % slope & intercept
    slope = coeffs(1);
    xFit = linspace(min(x), max(x), 100);
    yFit = polyval(coeffs, xFit);
    h2 = plot(xFit, yFit, '-', 'Color', [0.8 0.2 0.2], 'LineWidth', 1.2);

    %% Spearman rank correlation for slope significance
    [~, p_slope] = corr(x, y, 'Type', 'Spearman'); 

    %% One-sample t-test for mean Δ
    [~, p_mean] = ttest(y, 0);
    sigStr = '';
    if p_mean < 0.05
        sigStr = '*';
    end

    %% Scatter and zero line
    scatter(x, y, 20, 'MarkerFaceColor', ptColor, 'MarkerEdgeColor', 'none', 'MarkerFaceAlpha', 0.35);
    h1 = yline(0, '--', 'Color', [0.45 0.45 0.45], 'LineWidth', 1.5);

    %% Legend
    legend([h1 h2], {'No change','Regression'}, 'Location','southwest', ...
           'FontName','Times New Roman','FontSize',9);

    %% Axis formatting
    xlim([min(x)*0.95, max(x)*1.05]);
    maxAbs = max(abs(y));
    ylim([-maxAbs maxAbs]*1.1);
    box on;
    set(gca, 'FontName', 'Arial', 'FontSize', 10);
    title(labelPrefix, 'FontWeight', 'bold', 'FontName', 'Times New Roman', 'FontSize',17);
    ylabel(ylab,'FontName','Times New Roman','FontSize',15);

    %% Stats text
    meanDelta = mean(y, 'omitnan');
    sdDelta   = std(y, 'omitnan');

    eqStr = sprintf(['slope = %.3f\np-slope = %.3f\n' ...
                     'mean Δ = %.3f\nsd Δ = %.3f\np-mean = %.3f %s'], ...
                     slope, p_slope, meanDelta, sdDelta, p_mean, sigStr);

    % Place text in top-right corner
    text(0.96, 0.96, eqStr, 'Units','normalized', ...
         'VerticalAlignment','top', 'HorizontalAlignment','right', ...
         'FontName','Times New Roman', 'FontWeight','bold', 'FontSize', 9, ...
         'Color','r', 'BackgroundColor',[1 1 1 0.7]);

end