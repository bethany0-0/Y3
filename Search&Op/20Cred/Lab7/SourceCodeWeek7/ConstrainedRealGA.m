function [best_fitness, best_ind] = ConstrainedRealGA(fname, NDim, MaxIter, PopSize)

%Input Arguments:
%   fname       - the name of the evaluation .m function
%   NDim        - dimension of the evalation function
%   MaxIter     - maximum iteration
%   PopSize     - number of individuals in the populatoin

% Real coded GA for Matlab
% Copyright (C) 2014-2016 Shan He, the University of Birmingham

close all;
%% Parameters
max_iter = MaxIter;
termination_flag = false;

% Mutation and crossover rates
p_m = 0.1;
p_c = 0.8;

% Ranking selection parameters
num_parent = floor(PopSize * 0.3);
alpha = 0.05;
beta = 2-alpha;
mu = PopSize;
%% Lower/Upper bounds
Bound=eval(fname);
% Defined lower bound and upper bound.
LowerBound = zeros(PopSize, NDim);
UpperBound = zeros(PopSize, NDim);
for i=1:PopSize
    LowerBound(i,:)=Bound(:,1);
    UpperBound(i,:)=Bound(:,2);
end

%% Initialize population randomly
population =  rand(PopSize, NDim).*(UpperBound-LowerBound) + LowerBound;

best_fitness = [];
avg_fitness = [];
t = 0;
while termination_flag == false
    
    %% Evaluation fitness
    fitness = [];
    original_fitness = [];
    Gsum = [];
    exexutefunction=strcat(fname,'(population(i,:))');
    for i = 1:PopSize,
        [fitness(i) original_fitness(i) G] = eval(exexutefunction);
        Gsum = [Gsum sum(G)];
    end
    
    % Perform truncation selection by selecting the top num_parent individuals as parents
    [fitness, sorted_idx] = sort(fitness);
    %[fitness, sorted_idx] = stochastic_ranking_sort(original_fitness, Gsum);
    population = population(sorted_idx,:);
    parents = population(1:num_parent,:);
    offerspring = parents;    
    
    %% apply crossover
    %  Whole arithmetical crossover
    for j=1:2:floor(num_parent/2)
        if rand(1) < p_c
            alpha1 = rand(1);
            alpha2 = 1 - rand(1);
            % Randomly select two individuals as parents
            idx  = randi([1 num_parent],2, 1);
            % Weighted sum
            offerspring(j, :) = alpha1*parents(idx(1,:), :) + alpha2*parents(idx(2,:), :);
            offerspring(j+1, :) = alpha2*parents(idx(1,:), :) + alpha1*parents(idx(2,:), :);
        end
    end
    
    
    %% apply mutation    
    % We use Gaussain mutation
    for j=1:num_parent
        sigma = (UpperBound(j,:)-LowerBound(j,:))*0.01;
        mu = parents(j,:);  
        % Generate a random number in the Gaussain distribution with mean
        % \mu = x_i and sigma = (Ui- Li)*0.01
        temp_offspring = randn(1,NDim) .* sigma + mu;
        offerspring(j, :) = min(max(temp_offspring, LowerBound(j,:)), UpperBound(j,:));
    end
    %% Replace the worst individuals
    population(end:-1:end-num_parent+1,:) = offerspring;
    
    
    %% Results statistics
    best_ind = population(1,:);
    exexutefunction=strcat(fname,'(best_ind)');
    [f_new best_fitness G] = eval(exexutefunction);    
    disp(['Best solution is: ', num2str(best_ind), '; Best fitenss = ', num2str(best_fitness)]);
    disp(['The sum of G : ', num2str(sum(G))]);
    
    %     figure(2)
    %     plot(avg_fitness(1:t),'r--');xlabel('iteration'); ylabel('Average f(x)');
    %     text(0.5,0.95,['Best = ', num2str(fitness(1))],'Units','normalized');
    %     drawnow;
    %% Terminate?
    t=t+1;
    if(t>max_iter)
        termination_flag = true;
    end
    
    
end

end


