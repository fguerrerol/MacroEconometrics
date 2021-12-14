clear
clc
close all;

mat_names={'MVF_gap'}; %nombre de los matfiles guradados de la salida de dynare

vars=    {'y'  ,'gYBar'      ,'gY_obs'}; % variables que queremos graficar, con el nombre segun se definio en dynare
vars_tex={'Gap','Pot. Growth','gY_obs'}; % nombre de las mismas vraibales de arriba, pero para mostrar en el grafico

date_ini=2001; % fecha inicial de la muestra, que si es trimestral es formato, por ejemplo 2001, 2001.25, 2001.5, 2001.75
date_freq='q'; % frecuencia de los datos

plot_bands=1; % igual a 1 si queremos que muestre la incertidumbre obtenida del smoother

% A partir de aqui no tocar

for j=1:max(size(vars))
    figure('Name',['Smoothed ' vars{j}],'units','normalized','outerposition',[0 0 1 1])
    set(gcf,'WindowStyle');
    for i=1:max(size(mat_names))
        load([mat_names{i} '.mat'],'M_','oo_');
        if max(size(mat_names))>1
            subplot(1,max(size(mat_names)),i)
        end
        smoothed_ji=getfield(oo_.SmoothedVariables,vars{j});
        Nobs=max(size(smoothed_ji));
        if strcmp(date_freq,'a')==1
            tt=date_ini:date_ini+Nobs-1;
        elseif strcmp(date_freq,'q')==1
            tt=date_ini:0.25:date_ini+(Nobs-1)*0.25;
        end
        plot(tt,smoothed_ji,'-b','LineWidth',2)
        hold on;
        plot(tt,0*tt,':k')
        title(['Smoothed ' vars_tex{j}])
        xlim([min(tt) max(tt)])
        if plot_bands==1
            pos_ji=loc(M_.endo_names,vars{j});
            smoothed_std_ji=squeeze(oo_.Smoother.State_uncertainty(pos_ji,pos_ji,:)).^0.5;
            plot(tt,smoothed_ji+1.96*smoothed_std_ji,'--b',tt,smoothed_ji-1.96*smoothed_std_ji,'--b','LineWidth',2)
        end
        hold off;        
    end
end