function [fbestval,bestparticle] = PSO(fname, NDim, MaxIter, PopSize)

%% Particle Swarm Optimiser for Matlab
% Copyright (C) 2014-2016 Shan He, the University of Birmingham
% Input Arguments:
%   fname       - the name of the evaluation .m function
%   NDim        - dimension of the evalation function
%   MaxIter     - maximum iteration
%   PopSize     - number of particles
%% Please read the pseudo-code on pp.16 of my slides on 25th Nov. 


%% Parameters
c1 = .6;       % PSO parameter C1
c2 = .6;       % PSO parameter C2
w=0.8;          % Inertia weigth


%% Lower/Upper bounds
Bound=eval(fname);
% Defined lower bound and upper bound.
LowerBound = zeros(PopSize, NDim);
UpperBound = zeros(PopSize, NDim);
for i=1:PopSize
    LowerBound(i,:)=Bound(:,1);
    UpperBound(i,:)=Bound(:,2);
end

% Line 01: Initialise X, positions of particles
X =  rand(PopSize, NDim).*(UpperBound-LowerBound) + LowerBound;     % Initialize swarm X

%% Line 02: We need to initialise V
vmax = ones(PopSize, NDim);
for i=1:NDim
    vmax(:, i)=(UpperBound(:,i)-LowerBound(:,i))/2;
end
V = vmax.*rand(PopSize, NDim);       % Initialize V



Pi = X;            % Line 03: Initializing Best previous positions


flag=0;
iteration = 0;

%% Evaluate the particles
exexutefunction=strcat(fname,'(X(i,:))');
% Line 06: Evaluate initial X
for i = 1:PopSize,
    fvalue(i) = eval(exexutefunction);
end

fpbest = fvalue;      % Initializing the corresponding fitness values
% Finding best particle in initial X
[fbestval,index] = min(fvalue);    % Find the globe best

%% Line 07: Main loop
while(flag == 0) & (iteration < MaxIter)
    iteration = iteration +1;
    % Save current particle for visualisation
    previous_X = X; 
    % Line 08: Selet the best particle Pg
    Pg = X(index, :);
    for i=1:PopSize
        
        r1 = rand(1, NDim);
        r2 = rand(1, NDim);
        %% Lines 10-11 Your implementation of equations (1)-(2) in the slides (pp. 15)
        V(i,:) = w*V(i,:) + c1*r1.*(Pi(i,:) - X(i,:)) + c2*r2.*(Pg - X(i,:));
     
        X(i,:) = X(i,:) + V(i,:);
        
        
        %% Line 12: Prevent particles from flying outside search space
        OutFlag = X(i,:)<=LowerBound(i,:) | X(i,:)>=UpperBound(i,:);
        X(i,:) = X(i,:) - OutFlag.*V(i,:);
        
        
        %% Line 13: Evaluate the new swarm        
        fvalue(i) = eval(exexutefunction);
        
        %% You implementation for updating P_i 
        % Line 14: Updating the pbest for each particle
        if fvalue(i) <= fpbest(i)
           Pi(i,:) = X(i,:);
           fpbest(i) = fvalue(i);
            
            
        end
        
    end
    
    %% Updating index
    [fbestval, index] = min(fvalue);
    disp(['Best fitenss = ', num2str(fbestval)]);
    popvisualisation(fname, X, previous_X, iteration, MaxIter)
end

bestparticle = X(index, :)


