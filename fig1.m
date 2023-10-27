% Create a figure and axes with an equirectangular projection
figure;
ax = worldmap('World');
framem on;
gridm off;

% Customize the map background
setm(ax, 'FFaceColor', 'w'); % Set the oceans to white
setm(ax, 'GColor', 'k');
setm(ax, 'MeridianLabel', 'off'); % Turn off meridian labels
setm(ax, 'ParallelLabel', 'off'); % Turn off parallel labels

% Add coastlines with grey fill
load coastlines;
patchm(coastlat, coastlon, [0.7 0.7 0.7]); % Make continents grey

% Define the 5 arbitrary points (latitude and longitude pairs)
points = [
    40, -100; % Point 1
    0, 0;     % Point 2
    10, 20;   % Point 3
    -30, 60;  % Point 4
    -20, -80; % Point 5
];

% Plot the points on the map with filled black dots
for i = 1:size(points, 1)
    lat = points(i, 1);
    lon = points(i, 2);
    plotm(lat, lon, 'o', 'MarkerSize', 6, 'MarkerFaceColor', 'k', 'MarkerEdgeColor', 'k');
end
