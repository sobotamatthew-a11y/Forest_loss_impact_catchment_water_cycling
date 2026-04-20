%% Clear
clear; clc;

%% Directory
dataDir = 'D:\Forest_loss\HPC\result_041326\';

%% Load CSV files
ei1     = readtable(fullfile(dataDir,'rmse_results_EI1_1.csv'));
ei2     = readtable(fullfile(dataDir,'rmse_results_EI1_2.csv'));
ei5     = readtable(fullfile(dataDir,'rmse_results_EI1_5.csv'));
rb1     = readtable(fullfile(dataDir,'rmse_results_RB1_1.csv'));
rb2     = readtable(fullfile(dataDir,'rmse_results_RB1_2.csv'));
rb5     = readtable(fullfile(dataDir,'rmse_results_RB1_5.csv'));

%% Function to prepare table
function T = prepRun(T, label)
    T = sortrows(T,'Num_Features'); 
    T.Iteration = (1:height(T))';
    T.Run = repmat(string(label),height(T),1);
end

%% Color palette
colors = [
    249/255,166/255,2/255;      % 1 yellow
    59/255,177/255,67/255;      % 2 green (EI)
    58/255,67/255,186/255;      % 3 blue (RB)
    237/255,112/255,20/255;     % 4 orange
    208/255,49/255,45/255;      % 5 red (min)
    75/255,55/255,28/255;       % 6 dark brown
    0.5 0.5 0.5;                % 7 gray
    0.678,0.847,0.902;          % 8 light blue-gray
    0.00,0.45,0.74;             % 9 light blue
    0.85,0.33,0.10;             % 10 red-orange
    0.95,0.30,0.02;             % 11 deep red-orange
    0.31,0.03,0.49;             % 12 purple
    0.76,0.55,0.90];            % 13 light purple

%% Process datasets (adds Iteration and Run)
ei1     = prepRun(ei1,'EI1');
ei2     = prepRun(ei2,'EI2');
ei5     = prepRun(ei5,'EI5');
rb1     = prepRun(rb1,'RB1');
rb2     = prepRun(rb2,'RB2');
rb5     = prepRun(rb5,'RB5');

%% Datasets and titles
datasets = {ei1, ei2, ei5, rb1, rb2, rb5};
customTitles = ["ΔEI1", "ΔEI2", "ΔEI5", "ΔRB1", "ΔRB2", "ΔRB5"];
panelLabels = {'(a)','(b)','(c)','(d)','(e)','(f)'};

eiLineColor = colors(2,:);   % dark green
rbLineColor = colors(3,:);   % blue

%% Compute global y-limits
allRMSE = [];
for i = 1:length(datasets)
    allRMSE = [allRMSE; datasets{i}.RMSE];
end
ymin = min(allRMSE) * 0.98;
ymax = max(allRMSE) * 1.02;

allFeat = [];
for i = 1:length(datasets)
    allFeat = [allFeat; datasets{i}.Num_Features];
end

xmin = min(allFeat);
xmax = max(allFeat);

xOffset = 0.04;   % 2% from left
yOffset = 0.975;   % slightly below top (less than 1)

%% Plotting
figure;
set(gcf,'Units','inches','Position',[1 1 12 6])
set(gcf,'PaperUnits','inches')
set(gcf,'PaperPosition',[0 0 12 6])
set(gcf, 'Color', 'w');  % 'w' = white

t = tiledlayout(2,3,'TileSpacing','compact','Padding','compact');

for i = 1:length(datasets)
    nexttile
    T = datasets{i};
    
    % Find minimum RMSE
    [~, idx] = min(T.RMSE);
    mp = T(idx,:);
    
    % Assign line color
    if i <= 3
        lineColor = eiLineColor;  % EI
    else
        lineColor = rbLineColor;  % RB
    end

    % --- Plot line first ---
    plot(T.Num_Features, T.RMSE, 'Color', lineColor, 'LineWidth',2);
    hold on
    
    % Scatter points colored same as line
    scatter(T.Num_Features, T.RMSE, 40, lineColor, 'filled', 'MarkerFaceAlpha', 0.6);
    
    % Highlight minimum RMSE in red
    scatter(mp.Num_Features, mp.RMSE, 75, colors(5,:), 'filled', 'MarkerEdgeColor','k');
    
    % Set shared y-limits
    ylim([ymin ymax])
    xlim([xmin xmax])
    
    % Panel letter labels
text(xOffset, yOffset, panelLabels{i}, 'Units', 'normalized', ...
     'VerticalAlignment', 'top', 'HorizontalAlignment', 'left', ...
     'FontWeight','bold','FontSize',16,'FontName','Arial');

    % Label minimum
    yl = ylim;
    xl = xlim;
    labelStr = sprintf('Min RMSE: %.4f\nIteration: %d', mp.RMSE(1), mp.Iteration(1));
    text(xl(2)-0.02*(xl(2)-xl(1)), yl(2)-0.03*(yl(2)-yl(1)), labelStr, 'Color','r', 'FontSize', 12, 'FontWeight','bold','HorizontalAlignment','right','VerticalAlignment','top', 'FontName', 'Calibri');
    
    % Formatting
    set(gca, 'FontName', 'Helvetica', 'FontSize', 10);
    title(customTitles(i),'FontWeight','bold', 'FontName', 'Times New Roman', 'FontSize', 20);
    set(gca,'TickDir','in')
    xlabel(t,'Number of Features', 'FontName', 'Times New Roman','FontSize',20)
    ylabel(t,'RMSE', 'FontName','Times New Roman','FontSize',20)

    grid off
    box on

    % Remove x tick labels on top row
    if i <= 3
        set(gca,'XTickLabel',[])
    end

    if mod(i-1,3) ~= 0
    set(gca,'YTickLabel',[])
    end

end