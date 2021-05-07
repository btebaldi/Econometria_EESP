function [fator, loading, screenPlot, explained] = Fatores2(data, m)
    
    % determina o tamanho da matrix data
    [t,n]=size(data);
    
    [coeff,score,latent,tsquared,explained,mu] = pca(data, 'Algorithm', 'eig');
    
    x=1:size(latent, 1);
    screenPlot = figure('visible','off');
    plot(x, explained, '-', x , cumsum(explained), ':');
    legend({'Explained', 'Cumulative explained'})
    xlabel('Componente');
    ylabel('Explained');
    title('Scree plot');
    
    % Normaliza os autovetores de forma que loading'*loading /n = I
    loading = sqrt(n)*coeff(:,1:m);
    
    % Calcula os fatores baseado nos "Loadings"
    fator=data*loading/n;
end

