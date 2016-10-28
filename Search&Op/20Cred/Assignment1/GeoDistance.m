function d = GeoDistance(inputcities)

%x[i] and y[i] = coord for cities in long and lat
%convert to geographical distance
cities_num = length(inputcities);
coord = zeros(cities_num, 2);

%convert long and lat to to radians
PI = 3.141592;

for i=1 : cities_num

	deg = round(inputcities(i, 1));
	minimum = inputcities(i, 1) - deg;
	coord(i, 1) = PI * (deg + 5.0 * minimum / 3.0) / 180.0;

	deg = round(inputcities(i, 2));
	minumum = inputcities(i, 2) - deg;
	coord(i, 2) = PI * (deg + 5.0 * minimum / 3.0) / 180.0;

end

dist = zeros(1, cities_num);

%calculate distance
for i=1 : cities_num
	
	j = i+1;	
	
	if i == cities_num
		j = 1;
	end
			
	RRR = 6378.388;
	q1 = cos(coord(i, 2) - coord(j, 2));
	q2 = cos(coord(i, 1) - coord(j, 1));
	q3 = cos(coord(i, 1) + coord(j, 1));
	dist(1, i) = round(RRR * acos(0.5*((1.0+q1)*q2 - (1.0-q1)*q3) ) + 1.0);

end

% acos in inverse of cos

d = sum(dist);

end
