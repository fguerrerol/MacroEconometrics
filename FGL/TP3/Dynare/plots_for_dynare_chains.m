function plots_for_dynare_chains(model_name,blocks_mh,nrows,ncols,oo_)
% Reading estimated parameter names
para_estim_names=fieldnames(oo_.posterior_mode.parameters);

if isfield(oo_.posterior_mode,'shocks_std')==1
    aux=fieldnames(oo_.posterior_mode.shocks_std);
    for j=1:size(aux,1)
        aux(j,1)={['SE_' aux{j,1}]};
    end
    para_estim_names=[para_estim_names; aux];
end
if isfield(oo_.posterior_mode,'measurement_errors_std')==1
    aux=fieldnames(oo_.posterior_mode.measurement_errors_std);
    for j=1:size(aux,1)
        aux(j,1)={['SE_EOBS_' aux{j,1}]};
    end
    para_estim_names=[para_estim_names; aux];
end
N_para=size(para_estim_names,1);

N_blocks=size(blocks_mh,2);

for j=1:N_blocks
    load([model_name '\metropolis\' model_name '_mh1_blck' num2str(blocks_mh(j)) '.mat']);
    chain_para(:,:,j)=x2;
    chain_log_p(:,j)=logpo2;
end

Nchain=size(chain_para,1);

ngraphs=ceil((N_para+1)/(ncols*nrows));

for recursive=0:1
    nn=1;
    for j=1:ngraphs
        if recursive==0
            figure('Name',['Chains ' num2str(j) ])
        else
            figure('Name',['Recursive mean of Chains ' num2str(j) ])
        end
%         set(gcf,'WindowStyle','docked');
        for h=1:ncols*nrows
            if nn<N_para+1
                name_h=para_estim_names{nn};
                subplot(nrows,ncols,h)
                for b=1:N_blocks
                    chain_hb=squeeze(chain_para(:,nn,b));
                    if recursive==1
                        chain_hb=cumsum(chain_hb)./[1:Nchain]';
                    end
                    plot(1:Nchain,chain_hb)
                    hold on;
                end
                title(name_h, 'Interpreter', 'none')
            elseif nn==N_para+1 && recursive==0
                name_h='Log Post';
                subplot(nrows,ncols,h)
                for b=1:N_blocks
                    chain_hb=squeeze(chain_log_p(:,b));
                    plot(1:Nchain,chain_hb)
                    hold on;
                end
                title(name_h, 'Interpreter', 'none')
            end
            nn=nn+1;
        end
    end         
end
