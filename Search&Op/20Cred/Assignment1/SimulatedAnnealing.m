function [best_distance best_tour] = SimulatedAnnealing(inputcities)
% randomsearch

% Hill climbing search algorithm
%The input arguments are
% inputcities         - The cordinates for n cities are represented as 2
%                       rows and n columns and is passed as an argument for
%                       greedysearch.

%Parameters
%Geoistance = E()

t0 = 0.8;
kmax = 200;

num_cities = length(inputcities);

% Generate an initial solution
% You can generate a random solution as the inital solution. 
% If you execute your algorithm several times, you have the hill climbing
% algorithm with random restart. 
best_tour = [randperm(num_cities)];
best_cities_coordinates = inputcities(:,best_tour)
best_distance = GeoDistance(best_cities_coordinates);

k = 0;

while (k<kmax)
    T = t0/log(k);         %temperature function;
	
	for i = 2 : num_cities-1
        for j = i+2 : num_cities - 1
            
            	% Execute the swapping function
            	new_tour = twoopt(best_tour, i, j);
            	new_cities_coordinates = inputcities(:,new_tour);
            	new_distance = GeoDistance(new_cities_coordinates);
            	
		if P(best_distance, new_distance, T) > rand
                	best_distance = new_distance;
                	best_tour = new_tour;
                	plotcities(new_cities_coordinates);
                	% Since it is a simple hill climbing algorihtm,
                	% accept the first better solution and then terminate (break) 
                	% the search of other immediate neighbours
                	
                	break; 
                	
                	%%%%%%%%%%%5with break finds first best, leave in
                	%checks all k
                	% Question: how can you get a steepest ascent (descent)
                	% hill climbing algorithm?
            	end	
        end
    	end

	k = k+1;
end
end



