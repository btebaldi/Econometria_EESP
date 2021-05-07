pi = pi();
T = linspace(0,0.7,10);
space = 1:100;
MA = nan(size(space,2),10);
AR = nan(size(space,2),10);
for i=1:numel(space)
    for j=1:numel(T)
        MA(i, j) = (1/(2*pi))* (1+T(j)^2 + 2*T(j)*cos(pi*space(i)/size(space,2)));
        AR(i, j) = (1/(2*pi))* 1/(1+T(j)^2 - 2*T(j)*cos(pi*space(i)/size(space,2)));
    end
end

% for i=1:96
%     for j=1:numel(T)
%         x(i, j) = (1/(2*pi))* (1+T(j)^2 + 2*T(j)*cos(pi*i/96));
%     end
% end
subplot(1,2,1)
plot(space, AR(:,:))
ylim([0 2])
subplot(1,2,2)
plot(space, MA(:,:))
ylim([0 2])
% legend({'MA0', 'MA1', 'AR'})
