clearvars

f= @(n,i) i*n.^2 + (n.^2-n)*0.5 + n


x= 1:7

y1 = f(x,1)
y2 = f(x,2)
y3 = f(x,3)
y6 = f(x,6)
y12 = f(x,12)

plot(x, y1, x, y2, x, y3, x, y6, x, y12)
ylim([1 300])
% xlim([1 3])
xlabel('Qtd Variaveis')
ylabel('Parâmetros estimados')
legend({'VAR(1)','VAR(2)', 'VAR(3)', 'VAR(6)', 'VAR(12)'}, 'Location','northwest')
text(1:length(x),y1,num2str(y1'),'vert','bottom','horiz','center'); 
text(1:length(x),y12,num2str(y12'),'vert','bottom','horiz','center'); 
box off