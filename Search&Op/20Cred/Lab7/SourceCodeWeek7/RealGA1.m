function [best_fitness, best_ind] = RealGA1(fname, NDim, MaxIter, PopSize)

%Input Arguments:
%   fname       - the name of the evaluation .m function
%   NDim        - dimension of the evalation function
%   MaxIter     - maximum iteration

% Real coded GA for Matlab
% Copyright (C) 2014-2015 Shan He, the University of Birmingham

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

%% Visualise initial population
popvisualisation(fname, population, 1, max_iter);
%% Evaluate phenotypes
exexutefunction=strcat(fname,'(population(i,:))');
for i = 1:PopSize,
    fitness(i) = eval(exexutefunction);
end

[fitness, sorted_idx] = sort(fitness);
population = population(sorted_idx,:);

best_fitness = [];
avg_fitness = [];
t = 0;
while termination_flag == false
    
    %% selection
    num_parent = PopSize * 0.3;
    parents = population(1:num_parent,:);
    offerspring = parents;

    
%         % Perform ranking selection to select num_parents individuals as parents
%         [fitness, sorted_idx] = sort(fitness);
%         population = population(sorted_idx,:);
%         % gamma is now the ranks, i.e., the best one has the rank mu-1 and the
%         % worst one is 0
%         gamma = [mu-1:-1:0];
%         % Calculate the ranking function, i.e., the probability for selecting the
%         % top ranked individuals as parents.
%         p_gamma = (alpha + (beta-alpha).*gamma./(mu-1))./mu;
%         % Use a vector of \mu uniformly distributed random numbers to select num_parent
%         % individuals as parents.
%         selection_idx = p_gamma>=rand(1,mu);
%         parents = population(find(selection_idx==1),:);
%         % However, due to the randomness, we might not be able to select enough number
%         % of individuals as parents
%         while(size(parents,1)<num_parent)
%             selection_idx = p_gamma>=rand(1,mu);
%             temp = population(find(selection_idx==1),:);
%             parents = [parents; temp];
%         end

    %% apply crossover
    %     % Whole arithmetical crossover
    for j=1:2:floor(num_parent/2)
        if rand(1) < p_c
            % Generate a normally distributed random number
            alpha1 = rand(1);
            alpha2 = 1-alpha1;
            % Randomly select two individuals as parents
            % Your code here
            
            random = randi(length(parents));
            Parent1 = parents(random);
            random = randi(length(parents));
            Parent2 = parents(random);
            % Weighted sum using alpha1 and alpha2
            % Your code here
            offerspring(j, :) = alpha1*Parent1 + alpha2*Parent2;
            offerspring(j+1, :) = alpha2*Parent1 + alpha1*Parent2;
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
    
    %% Evaluation fitness
    fitness = [];
    exexutefunction=strcat(fname,'(population(i,:))');
    for i = 1:PopSize,
        fitness(i) = eval(exexutefunction);
    end
    
    [fitness, sorted_idx] = sort(fitness);
    population = population(sorted_idx,:);
    
    %% Results statistics
    best_fitness = fitness(1);
    avg_fitness = [avg_fitness mean(fitness)];
    best_ind = population(1,:);
    
    disp(['Best solution is: ', num2str(best_ind), '; Best fitenss = ', num2str(fitness(1))]);
    
    %     figure(2)
    %     plot(avg_fitness(1:t),'r--');xlabel('iteration'); ylabel('Average f(x)');
    %     text(0.5,0.95,['Best = ', num2str(fitness(1))],'Units','normalized');
    %     drawnow;
    %% Visualisation
  %  popvisualisation(fname, population, t, max_iter);
   % pause
    %% Terminate?
    t=t+1;
    if(t>max_iter)
        termination_flag = true;
    end
    
    
    
end


end


