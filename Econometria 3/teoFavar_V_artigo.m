%% Clear all data
clear all;
clc;
fprintf('Limpeza de variaveis completo.\n');
%% Load Data
load matlab2.mat

% xdata: data used to extract factors
% ydata: data on inflation, unemployment and interest rate
% tcode: Time code classification. For detrending
% slowcode: the slow or fast moving variables codes
% yearlab: the dates of the data (quarters)
% namesX: the short codes of the variables.
% SummaryTable: a summary of the X series.
fprintf('Carregamento de base de dados completo.\n');
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
fprintf('Estacionaridade das series completo.\n');
%% Centralizacao e padronizacao de variaveis

% centralização e padronização das series
X_c = (xdata - repmat(mean(xdata,1), size(xdata,1), 1));
X_std = X_c./repmat(std(X_c), size(xdata,1), 1); % First standardize data to extract PC


% Y: [inflation, unemployment, interest rate]
Y_c = (ydata - repmat(mean(ydata,1), size(ydata,1), 1));
Y_std=Y_c./repmat(std(Y_c), size(ydata,1), 1);
fprintf('Centralizacao e padronizacao completo.\n');
%% Deteerminacao de fatores e lag do VAR

% K: Quantidade de Fatores. Escolhido a priori
K = 4;

% p:  Dimensionality do VAR [Fatores, Y]
p = K+size(Y_c,2);

% Var_lag is number of lags in the VAR part
Var_lag = 2;
fprintf('Determinacao de LAG e Fatores completo.\n');
%% Estimacao de Fatores

% [fator, loading, screenPlot] = Fatores(data, m)
[Fator_0, ~, screenPlot, explained]=Fatores2(X_c, K);

% Mostra o Screen Test
screenPlot.Visible = 'on';
xlim([1 10]);

cumsumEx = 0;
for i=1:numel(explained)
    cumsumEx = cumsumEx + explained(i);
    fprintf('Fator %2.0d: %3.2f\n', i, cumsumEx)
end
clear cumsumEx i

fprintf('Estimacao de fatores completo.\n');
%% Realiza a rotação dos fatores segundo Bernanke, Boivin and Eliasz (2005)

% Busca as variáveis que sao "Slow-moving"
slowIndex = find(slowcode==1)';
X_slow = X_c(:,slowIndex);

% Busca os fatores das variaveis "Slow-moving"
[Fator_slow, ~, ~] = Fatores2(X_slow, K);
Fator_t = FatorFiltrado(Fator_0, Y_c(:,end), Fator_slow);

% determina os loadings referente ao fatores atravez de regressao OLS.
Loadings_t = (Fator_t'*Fator_t)\Fator_t'* X_c;

fprintf('Rotacao de fatores completo.\n');
%% Determina o FAVAR

%utilizado apenas na selecao de lag
Var_lag = 1;
inicio = 5;

% Determina a variavel
FAVAR = [Fator_t(inicio:end,:), Y_c(inicio:end,:)];

% determina o modelo
Mdl_favar = varm(p, Var_lag);
Mdl_favar.SeriesNames = {'Factor 1','Factor 2', 'Factor 3','Factor 4', 'Inflation' , 'Unemployment', 'Fed_funds'};

% Realiza a estimação do modelo
EstMdl_favar = estimate(Mdl_favar, FAVAR);
results_favar = summarize(EstMdl_favar);

fprintf ('Var Lag: %d\tSampleSize: %d \tAIC: %f \tBIC:%f\n',Var_lag, results_favar.SampleSize, results_favar.AIC, results_favar.BIC)


% calcula a resposta a impulso.
Y_imp_favar = armairf(EstMdl_favar.AR,{},'InnovCov',EstMdl_favar.Covariance);
X_imp_favar = Y_imp_favar(:, 1:K, p) * Loadings_t;

fprintf('Modelo FAVAR completo.\n');
%% Determina o VAR

% Determina a variavel
VAR = Y_c;

% determina o modelo
Mdl_var = varm(p-K, Var_lag);
Mdl_var.SeriesNames = {'Inflation' , 'Unemployment', 'Fed_funds'};

% Realiza a estimação do modelo
EstMdl_var = estimate(Mdl_var, VAR);

% calcula a resposta a impulso.
Y_imp_var = armairf(EstMdl_var.AR,{},'InnovCov',EstMdl_var.Covariance);

fprintf('Modelo VAR completo.\n');

%% Plots FAVAR

% Determina o total de Pontos plotados
nPlot = size(Y_imp_favar, 1);
%nPlot = 21
x = 1:nPlot;

% Plot 1: Impulse resposta de: inflation, unemployment, interest
figure
subplot(3,1,1)
plot(x, Y_imp_favar(x,K+1,p))
hold;
plot(zeros(1,nPlot),'-', 'Color', [0,0,0])
title('FAVAR Impulse response of inflation')
xlim([1 nPlot])

subplot(3,1,2)
plot(x, Y_imp_favar(x,K+2,p))
hold;
plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
title('FAVAR Impulse response of unemployment')
xlim([1 nPlot])

subplot(3,1,3)
plot(x, Y_imp_favar(x,K+3,p))
hold;
plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
title('FAVAR Impulse response of interest rate')
xlim([1 nPlot])

% Plot 2: Impulse resposta das demais variaveis
figure
% Escolhemos aqui 12 series
var_numbers = [9 12 61 59 92 102];
% short codes das variaveis escolhidas
var_names = namesX(var_numbers);

for i=1:6
    subplot(3, 2, i)
    plot(x, X_imp_favar(x, var_numbers(i)));
    hold;
    plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
    title(var_names(i))
    xlim([1 nPlot])
end

fprintf('Plotagem FAVAR completo.\n');

%% Plots VAR vs FAVAR

% Determina o total de Pontos plotados
nPlot = min(size(Y_imp_var, 1),size(Y_imp_favar, 1));
%nPlot = 21
x = 1:nPlot;

% Plot 1: Impulse resposta de: inflation, unemployment, interest
figure
subplot(3,1,1)
plot(x, Y_imp_var(x,1,3), x, Y_imp_favar(x,5,7))
hold;
plot(zeros(1,nPlot),'-', 'Color', [0,0,0])
title('VAR Impulse response of inflation')
legend({'VAR', 'FAVAR'})
xlim([1 nPlot])

subplot(3,1,2)
plot(x, Y_imp_var(x,2,3), x, Y_imp_favar(x,6,7))
hold;
plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
title('VAR Impulse response of unemployment')
legend({'VAR', 'FAVAR'})
xlim([1 nPlot])

subplot(3,1,3)
plot(x, Y_imp_var(x,3,3), x, Y_imp_favar(x,7,7))
hold;
plot(zeros(1,nPlot),'-', 'Color',[0,0,0])
title('VAR Impulse response of interest rate')
legend({'VAR', 'FAVAR'})
xlim([1 nPlot])

fprintf('Plotagem VAR completo.\n');
