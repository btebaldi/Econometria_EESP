function [fator, loading, screenPlot] = Fatores(data, m)
   
    % determina o tamanho da matrix data
    [t,n]=size(data);
    
    % Calcula W = Data' * Data
    W=data'*data;
    
    % Calcula os Autovetores e Autovalores
    [A_Vetores,A_Valores]=eig(W);
    
    % Ordena os autovalores do maior para o menor
    A_Valores = diag(A_Valores);
    [~, Index]=sort(A_Valores);
    Index=flipud(Index);
    
    A_Vetores_Ordenado=nan(n);
    for i=1:n
        A_Vetores_Ordenado(:,i)=A_Vetores(:,Index(i));
    end
    
    screenPlot = figure('visible','off');
    plot(1:size(A_Valores, 1), A_Valores(Index), '-');
    xlabel('Componente');
    ylabel('Autovalor');
    title('Screen plot');
    
    % Normaliza os autovetores de forma que loading'*loading /n = I
    loading = sqrt(n)*A_Vetores_Ordenado(:,1:m);
    
    % Calcula os fatores baseado nos "Loadings"
    fator=data*loading/n;
end

