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

% My initilising optimal solution so far
optimal = previous_distance;

while iterations < max_iter
    % You need to write code to generate a random solution
	order = randperm(num_cities); %doesn't create a unique order
	temp_cities = inputcities(:,order);

	% Evaluate the solution
    	current_distance = distance(temp_cities);
	
	% You need to write code to save the best solution
	if current_distance < optimal
       	 	optimal = current_distance;
    	end
	
	% Update interation
    iterations = iterations + 1;
    results(iterations) = current_distance;
    
    plot(results(1:iterations),'r--');xlabel('iteration'); ylabel('f(x)');
    text(0.5,0.95,['Best = ', num2str(optimal)],'Units','normalized');
    drawnow;
    
end
%My edited to return optimal distance
total_distance = optimal;
