filename1 = 'CMap_Fig1.xlsx';
dataTable = readtable(filename1);
c_flux = table2array(dataTable(:, 2));
c_biome = table2array(dataTable(:, 7));
c_extrf = c_flux(c_biome == 0);
c_des = c_flux(c_biome == 1);
c_step = c_flux(c_biome == 2);
c_tropf = c_flux(c_biome == 3);
c_tund = c_flux(c_biome == 4);

filename2 = 'NMap_Fig1.xlsx';
dataTable = readtable(filename2);
n_flux = table2array(dataTable(:, 2));

sz_c = size(c_flux);
sz_c = sz_c(:,1);
sz_c_extrf = size(c_extrf);
sz_c_extrf = sz_c_extrf(:,1);
sz_c_des = size(c_des);
sz_c_des = sz_c_des(:,1);
sz_c_step = size(c_step);
sz_c_step = sz_c_step(:,1);
sz_c_tropf = size(c_tropf);
sz_c_tropf = sz_c_tropf(:,1);
sz_c_tund = size(c_tund);
sz_c_tund= sz_c_tund(:,1);

sz_n = size(n_flux);
sz_n = sz_n(:,1);

% Define the number of bootstrap samples
num_bootstrap_samples = 1000;

% Initialize a matrix to store the bootstrap samples
bootstrap_samples_c = zeros(num_bootstrap_samples, sz_c);
bootstrap_samples_c_extrf = zeros(num_bootstrap_samples, sz_c_extrf);
bootstrap_samples_c_des = zeros(num_bootstrap_samples, sz_c_des);
bootstrap_samples_c_step = zeros(num_bootstrap_samples, sz_c_step);
bootstrap_samples_c_tropf = zeros(num_bootstrap_samples, sz_c_tropf);
bootstrap_samples_c_tund = zeros(num_bootstrap_samples, sz_c_tund);
bootstrap_samples_n = zeros(num_bootstrap_samples, sz_n);

% Perform bootstrap resampling
for i = 1:num_bootstrap_samples
    bootstrap_indices_c = randi(sz_c, 1, sz_c);
    bootstrap_sample_c = c_flux(bootstrap_indices_c);
    bootstrap_samples_c(i, :) = bootstrap_sample_c;
end

for i = 1:num_bootstrap_samples
    bootstrap_indices_c_extrf = randi(sz_c_extrf, 1, sz_c_extrf);
    bootstrap_sample_c_extrf = c_flux(bootstrap_indices_c_extrf);
    bootstrap_samples_c_extrf(i, :) = bootstrap_sample_c_extrf;
end

for i = 1:num_bootstrap_samples
    bootstrap_indices_c_des = randi(sz_c_des, 1, sz_c_des);
    bootstrap_sample_c_des = c_flux(bootstrap_indices_c_des);
    bootstrap_samples_c_des(i, :) = bootstrap_sample_c_des;
end

for i = 1:num_bootstrap_samples
    bootstrap_indices_c_step = randi(sz_c_step, 1, sz_c_step);
    bootstrap_sample_c_step = c_flux(bootstrap_indices_c_step);
    bootstrap_samples_c_step(i, :) = bootstrap_sample_c_step;
end

for i = 1:num_bootstrap_samples
    bootstrap_indices_c_tropf = randi(sz_c_tropf, 1, sz_c_tropf);
    bootstrap_sample_c_tropf = c_flux(bootstrap_indices_c_tropf);
    bootstrap_samples_c_tropf(i, :) = bootstrap_sample_c_tropf;
end

for i = 1:num_bootstrap_samples
    bootstrap_indices_c_tund = randi(sz_c_tund, 1, sz_c_tund);
    bootstrap_sample_c_tund = c_flux(bootstrap_indices_c_tund);
    bootstrap_samples_c_tund(i, :) = bootstrap_sample_c_tund;
end

for i = 1:num_bootstrap_samples
    bootstrap_indices_n = randi(sz_n, 1, sz_n);
    bootstrap_sample_n = n_flux(bootstrap_indices_n);
    bootstrap_samples_n(i, :) = bootstrap_sample_n;
end

% Calculate the average of each bootstrap sample
bootstrap_sample_averages_c = mean(bootstrap_samples_c, 2);
bootstrap_sample_averages_c_extrf = mean(bootstrap_samples_c_extrf, 2);
bootstrap_sample_averages_c_des= mean(bootstrap_samples_c_des, 2);
bootstrap_sample_averages_c_step= mean(bootstrap_samples_c_step, 2);
bootstrap_sample_averages_c_tropf= mean(bootstrap_samples_c_tropf, 2);
bootstrap_sample_averages_c_tund= mean(bootstrap_samples_c_tund, 2);
bootstrap_sample_averages_n = mean(bootstrap_samples_n, 2);

figure;

% Plot 1
subplot(2, 2, 1);
histogram(bootstrap_sample_averages_c, 'Normalization', 'probability', 'EdgeColor', 'k', 'LineWidth', 1.5, 'NumBins', 20);
title('C Bootstrapped');
xlabel('Average Value');
ylabel('Probability');

% Plot 2
subplot(2, 2, 2);
histogram(bootstrap_sample_averages_n, 'Normalization', 'probability', 'EdgeColor', 'k', 'LineWidth', 1.5, 'NumBins', 20);
title('N Bootstrapped');
xlabel('Average Value');
ylabel('Probability');

% Plot 3
subplot(2, 2, 3);
histogram(bootstrap_sample_averages_c_step, 'Normalization', 'probability', 'EdgeColor', [0.8,0.8,0.8], 'LineWidth', 1.5, 'NumBins', 20, 'FaceColor', [1,1,0.8]);
hold on;
histogram(bootstrap_sample_averages_c_tropf, 'Normalization', 'probability', 'EdgeColor', [0.7804,0.9137,0.7059], 'LineWidth', 1.5, 'NumBins', 20, 'FaceColor', [0.498,0.8039,0.7333]);
hold on;
histogram(bootstrap_sample_averages_c_tund, 'Normalization', 'probability', 'EdgeColor', [0.498,0.8039,0.7333], 'LineWidth', 1.5, 'NumBins', 20, 'FaceColor', [0.7804,0.9137,0.7059]);
hold on;
histogram(bootstrap_sample_averages_c_extrf, 'Normalization', 'probability', 'EdgeColor', [0.1725,0.498,0.7216], 'LineWidth', 1.5, 'NumBins', 20, 'FaceColor', [0.1725,0.498,0.7216]);
hold on;
histogram(bootstrap_sample_averages_c_des, 'Normalization', 'probability', 'EdgeColor', [0.1451,0.2039,0.7804], 'LineWidth', 1.5, 'NumBins', 20, 'FaceColor', [0.1451,0.2039,0.7804]);

title('Plot 3');
xlabel('Average Value');
ylabel('Probability');
%% legend('c\_step', 'c\_tropf'); 

% Plot 4
subplot(2, 2, 4);
histogram(bootstrap_sample_averages_c, 'Normalization', 'probability', 'EdgeColor', 'k', 'LineWidth', 1.5);
title('Plot 4');
xlabel('Average Value');
ylabel('Probability');