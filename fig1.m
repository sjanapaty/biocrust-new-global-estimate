figure;
ax = worldmap('World');
framem on;
gridm off;

setm(ax, 'FFaceColor', 'w'); % oceans = white
setm(ax, 'GColor', 'k');
setm(ax, 'MeridianLabel', 'off'); 
setm(ax, 'ParallelLabel', 'off'); 

load coastlines;
patchm(coastlat, coastlon, [0.7 0.7 0.7]); % continents = gray

filename = 'CMap_fig1.xlsx';
dataTable = readtable(filename);
lat = table2array(dataTable(:, 5));
lon = table2array(dataTable(:, 6));
elbert = table2array(dataTable(:, 8));

for i = 1:size(lat)
    if elbert(i) == 1
        plotm(lat(i), lon(i), 'o', 'MarkerSize', 8, 'MarkerFaceColor', [0.35 0.35 0.35], 'MarkerEdgeColor', 'k');
    else
        plotm(lat(i), lon(i), 'o', 'MarkerSize', 8, 'MarkerFaceColor', [0.8 0 0], 'MarkerEdgeColor', 'k');
    end
end