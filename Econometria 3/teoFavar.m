
%% Clear all data
clear all;
clc;
%% Load Data
load matlab.mat

% xdata: data used to extract factors
% ydata: data on inflation, unemployment and interest rate
% tcode: Time code classification. For detrending
% slowcode: the slow or fast moving variables codes
% yearlab: the dates of the data (quarters)
% namesX: the short codes of the variables.
% SummaryTable: a summary of the X series.

%% Deixa as series estacionarias segundo a classificação do Time Code

% Transform data to be approximately stationary
x_temp = nan(size(xdata));
for i_x = 1:size(xdata,2)   % Transform "X"
    x_temp(:,i_x) = TransformX(xdata(:,i_x),tcode(i_x));
end

% Corrige o tamanho das series depois de deixar as series estacionárias
xdata = x_temp(3:end,:);
ydata = ydata(3:end,:);
yearlab = yearlab(3:end);

% remove variaveis nao utilizadas
clear x_temp

%% Centralizacao e padronizacao de variaveis

% centralização e padronização das series
X_c = (xdata - repmat(mean(xdata,1), size(xdata,1), 1));
X_std = X_c./repmat(std(X_c), size(xdata,1), 1); % First standardize data to extract PC

% Y: [inflation, unemployment, interest rate]
Y_c = (ydata - repmat(mean(ydata,1), size(ydata,1), 1));
Y_std=Y_c./repmat(std(Y_c), size(ydata,1), 1);

%% Deteerminacao de fatores e lag do VAR

% K: Quantidade de Fatores. Escolhido a priori
K = 2;

% p:  Dimensionality do VAR [Fatores, Y]
p = K+size(Y_c,2);

% Var_lag is number of lags in the VAR part
Var_lag = 2;

%% Estimacao de Fatores

% [fator, loading, screenPlot] = Fatores(data, m)
[Fator_0, ~, screenPlot]=Fatores2(X_std, K);

% Mostra o Screen Test
screenPlot.Visible = 'on';
xlim([1 10]);

%% Realiza a rotação dos fatores segundo Bernanke, Boivin and Eliasz (2005)

% Busca as variáveis que sao "Slow-moving"
slowIndex = find(slowcode==1)';
X_slow = X_c(:,slowIndex);

% Busca os fatores das variaveis "Slow-moving"
[Fator_slow, ~, ~] = Fatores2(X_slow, K);
Fator_t = FatorFiltrado(Fator_0, Y_c(:,end), Fator_slow);

% determina os loadings referente ao fatores atravez de regressao OLS.
Loadings_t = (Fator_t'*Fator_t)\Fator_t'* X_c;


%% Determina o VAR

% Determina a variavel
Var = [Fator_t, Y_c];

% determina o modelo
Mdl = varm(p, Var_lag);
Mdl.SeriesNames = {'Factor 1','Factor 2', 'Inflation' , 'Unemployment', 'Fed_funds'};

% Realiza a estimação do modelo
EstMdl = estimate(Mdl, Var);
summarize(EstMdl)

% calcula a resposta a impulso.
Y_imp = armairf(EstMdl.AR,{},'InnovCov',EstMdl.Covariance);
X_imp = Y_imp(:, 1:2, p) * Loadings_t;


%% Calculando os intervalos de confianca

% rng(1); % For reproducibility
% YSim = simulate(EstMdl2,10,'Y0',Data{idxF,:},'NumPaths',2000);

%% Plots

% Determina o total de Pontos plotados
nPlot = size(Y_imp, 1);

nPlot = 21
x = 1:nPlot;

% Plot 1: Impulse resposta de: inflation, unemployment, interest
figure
subplot(3,1,1)
plot(x, Y_imp(x,3,p))
hold;
plot(zeros(1,nPlot),'-', 'Color', [0,0,0])
title('Impulse response of inflation')
xlim([1 nPlot])

subplot(3,1,2)
plot(x, Y_imp(x,4,p))
hold;
plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
title('Impulse response of unemployment')
xlim([1 nPlot])

subplot(3,1,3)
plot(x, Y_imp(x,5,p))
hold;
plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
title('Impulse response of interest rate')
xlim([1 nPlot])

% Plot 2: Impulse resposta das demais variaveis
figure
% Escolhemos aqui 12 series
var_numbers = [2  9 10 28 42 46 77 91 92 102 109 111];
% short codes das variaveis escolhidas
var_names = namesX(var_numbers);

for i=1:12
    subplot(4, 3, i)
    plot(x, X_imp(x, var_numbers(i)));
    hold;
    plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
    title(var_names(i))
    xlim([1 nPlot])
end
