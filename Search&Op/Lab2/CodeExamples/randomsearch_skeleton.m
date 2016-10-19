function total_distance = randomsearch_skeleton(inputcities,max_iter)
% randomsearch

% Randomised search algorithm for TSP problem
%The input arguments are
% inputcities         - The cordinates for n cities are represented as 2
%                       rows and n columns and is passed as an argument for
%                       the randomised search algorithm.
% max_iter           - max_iter is the stopping criteria  

global iterations;
% Initialize the iteration number.
iterations = 1;

num_cities = length(inputcities);

% Objective function: the total distance for the routes.
previous_distance = distance(inputcities);
results = zeros(max_iter,1);
results(1) = previous_distance;
plot(results);
while iterations < max_iter
    % You need to write code to generate a random solution

	
	% Evaluate the solution
    current_distance = distance(temp_cities);
	
	% You need to write code to save the best solution

	
	% Update interation
    iterations = iterations + 1;
    results(iterations) = current_distance;
    
    plot(results(1:iterations),'r--');xlabel('iteration'); ylabel('f(x)');
    text(0.5,0.95,['Best = ', num2str(current_distance)],'Units','normalized');
    drawnow;
    
end

total_distance = previous_distance;