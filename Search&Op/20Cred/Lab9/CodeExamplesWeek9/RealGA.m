function [best_fitness, best_ind] = RealGA(fname, NDim, MaxIter, PopSize)

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
        [fitness(i) ] = eval(exexutefunction);
    end
    
    % Perform truncation selection by selecting the top num_parent individuals as parents
    [fitness, sorted_idx] = sort(fitness);
    population = population(sorted_idx,:);
    parents = population(1:num_parent,:);
    offerspring = parents;
    
    %     % Perform ranking selection to select num_parents individuals as parents
    %     [fitness, sorted_idx] = sort(fitness);
    %     population = population(sorted_idx,:);
    %     % gamma is now the ranks, i.e., the best one has the rank mu-1 and the
    %     % worst one is 0
    %     gamma = [mu-1:-1:0];
    %     % Calculate the ranking function, i.e., the probability for selecting the
    %     % top ranked individuals as parents.
    %     p_gamma = (alpha + (beta-alpha).*gamma./(mu-1))./mu;
    %     % Use a vector of \mu uniformly distributed random numbers to select num_parent
    %     % individuals as parents.
    %     selection_idx = p_gamma>=rand(1,mu);
    %     parents = population(find(selection_idx==1),:);
    %     % However, due to the randomness, we might not be able to select enough number
    %     % of individuals as parents
    %     while(size(parents,1)<num_parent)
    %         selection_idx = p_gamma>=rand(1,mu);
    %         temp = population(find(selection_idx==1),:);
    %         parents = [parents; temp];
    %     end
    
    
    %% apply crossover
    %  Whole arithmetical crossover
    for j=1:2:floor(num_parent/2)
        if rand(1) < p_c
            alpha = rand(1);
            % Randomly select two individuals as parents
            idx  = randi([1 num_parent],2, 1);
            % Weighted sum
            offerspring(j, :) = alpha*parents(idx(1,:), :) + (1-alpha)*parents(idx(2,:), :);
            offerspring(j+1, :) = alpha*parents(idx(2,:), :) + (1-alpha)*parents(idx(1,:), :);
        end
    end
    
    
    % We use Gaussain mutation, but you can copy and paste the non-uniform mutation
    % you have implemented in week 5
    for j=1:num_parent
        sigma = (UpperBound(j,:)-LowerBound(j,:))/100;
        mu = parents(j,:);      
        temp_offspring = randn(1,NDim) .* sigma + mu;
        offerspring(j, :) = min(max(temp_offspring, LowerBound(j,:)), UpperBound(j,:));
    end
    %% Replace the worst individuals
    population(end:-1:end-num_parent+1,:) = offerspring;
    
    
    %% Results statistics
    best_ind = population(1,:);
    exexutefunction=strcat(fname,'(best_ind)');
    [best_fitness] = eval(exexutefunction);    
    disp(['Best fitenss = ', num2str(best_fitness)]);
    
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


