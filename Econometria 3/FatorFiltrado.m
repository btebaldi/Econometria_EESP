function FatorFilter = FatorFiltrado(F, Y, F_slow)

    % Determina o tamanho do 
    k1=size(Y,2);

    % Determina b atravez de regressao OLS
    X = [ones(size(Y,1),1) Y F_slow];
    b = (X'*X)\X'*F;

    FatorFilter = F - Y * b(2:k1+1,:);
    
end % End of function FatorRotate