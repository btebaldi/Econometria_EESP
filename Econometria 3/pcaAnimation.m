%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Matlab code to produce PCA animations shown here:
% http://stats.stackexchange.com/questions/2691
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Static image

clear all
rng(42)

X = randn(100,2);
scatter(X(:,1), X(:,2), 'b', 'filled')

X = X*chol([1 0.6; 0.6 0.6]);
scatter(X(:,1), X(:,2), 'b', 'filled')

X = bsxfun(@minus, X, mean(X));
scatter(X(:,1), X(:,2), 'b', 'filled')

[a,b] = eig(cov(X));

[coeff,score,latent,tsquared,explained,mu] = pca(X, 'NumComponents', 2)
jj = -4:4;

% figure
% scatter3(X(:,1), X(:,2), score(:,1))
% 
% figure
% scatter3(X(:,1), X(:,2), zeros(size(X(:,2))))

[L2,T] = rotatefactors(coeff,'method','promax','power',4);

s2= score*T



fig = figure('Position', [100 100 1000 400]);
set(gcf,'color','w');
axis([-3.5 3.5 -3.5 3.5])
axis square
hold on
scatter(X(:,1), X(:,2), 'b', 'filled')
scatter(coeff(1,1)*score(:,1), coeff(2,1)*score(:,1), 'r', 'filled')
scatter(coeff(1,2)*score(:,2), coeff(2,2)*score(:,2), 'r', 'filled')
scatter(L2(1)*s2(:,1), L2(2)*s2(:,1), 'r', 'filled')



%% Rotating animation

fig = figure('Position', [100 100 1000 400]);
set(gcf,'color','w');
axis([-3.5 3.5 -3.5 3.5])
axis square
hold on

for alpha = 1:1:179
    w = [cosd(alpha) sind(alpha)];
    z = X*w'*w;
    
    cla
    for i=1:100
        plot([X(i,1) z(i,1)], [X(i,2) z(i,2)], 'r')
    end
    
    plot(w(1)*3.5*[-1 1], w(2)*3.5*[-1 1], 'k')
    plot(-w(2)*2*[-1 1], w(1)*2*[-1 1], 'Color', [.6 .6 .6])
    
    scatter(z(:,1), z(:,2), 'r', 'filled')
    scatter(X(:,1), X(:,2), 'b', 'filled')
    scatter(0,0,65,'k','filled', 'LineWidth', 2, 'MarkerFaceColor', [1 1 1], 'MarkerEdgeColor', [0 0 0])
    
    a1 = 3.5;
    a2 = 4.5;
    plot(a(1,2)*[-a2 -a1], a(2,2)*[-a2 -a1], 'm', 'LineWidth', 2)
    plot(a(1,2)*[ a1  a2], a(2,2)*[ a1  a2], 'm', 'LineWidth', 2)
    drawnow
    
    frame = getframe(fig);
    if alpha == 1
        [imind,map] = rgb2ind(frame.cdata,16,'nodither');
    else
        imind(:,:,1,alpha) = rgb2ind(frame.cdata,map,'nodither');
    end
end
imwrite(imind,map, 'animation_pca.gif', 'DelayTime', 0, 'LoopCount', inf)

%% Pendulum animation

fig = figure('Position', [100 100 1000 400]);
set(gcf,'color','w');
axis([-3.5 3.5 -3.5 3.5])
axis square
hold on

alpha = -45;
omega = 0;
for t = 1:1:1000
    w = [cosd(alpha) sind(alpha)];
    z = X*w'*w;
    
    M = sum(sum((z * [0 1; -1 0]) .* (X-z), 2));
    omega = omega + M;
    omega = omega * 0.93;
    alpha = alpha + omega/40;
    if(abs(omega)<1 && abs(alpha-abs(atand(a(2,2)/a(1,2)))) < 1)
        break
    end
    
    cla
    for i=1:100
        plot([X(i,1) z(i,1)], [X(i,2) z(i,2)], 'r')
    end
    
    plot(w(1)*3.5*[-1 1], w(2)*3.5*[-1 1], 'k')
    plot(-w(2)*2*[-1 1], w(1)*2*[-1 1], 'Color', [.6 .6 .6])
    
    scatter(z(:,1), z(:,2), 'r', 'filled')
    scatter(X(:,1), X(:,2), 'b', 'filled')
    scatter(0,0,65,'k','filled', 'LineWidth', 2, 'MarkerFaceColor', [1 1 1], 'MarkerEdgeColor', [0 0 0])
    
    a1 = 3.5;
    a2 = 4.5;
    plot(a(1,2)*[-a2 -a1], a(2,2)*[-a2 -a1], 'm', 'LineWidth', 2)
    plot(a(1,2)*[ a1  a2], a(2,2)*[ a1  a2], 'm', 'LineWidth', 2)
    
    pause(0.01)
    
    drawnow
    
    frame = getframe(fig);
    if t == 1
        [imind,map] = rgb2ind(frame.cdata,16,'nodither');
    else
        imind(:,:,1,t) = rgb2ind(frame.cdata,map,'nodither');
    end
end
imwrite(imind,map, 'animation_pca_pendulum.gif', 'DelayTime', 0, 'LoopCount', inf)