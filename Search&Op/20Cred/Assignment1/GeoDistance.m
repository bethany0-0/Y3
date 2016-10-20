function d = GeoDistance(inputcities)

%x[i] and y[i] = coord for cities in long and lat
%convert to geographical distance
cities_num = length(inputcities);
coord = zeros(2, cities_num);

%convert long and lat to to radians
PI = 3.141592;

for i=1 : cities_num

	deg = round(inputcities(1,i));
	minimum = inputcities(1,i) - deg;
	coord(1,i) = PI * (deg + 5.0 * minimum / 3.0) / 180.0;

	deg = round(inputcities(2,i));
	minumum = inputcities(2,i) - deg;
	coord(2,i) = PI * (deg + 5.0 * minimum / 3.0) / 180.0;

end

dist = zeros(1,cities_num);

%calculate distance
for i=1 : cities_num
	
	j = i+1;	
	
	if i == cities_num
		j = 1;
	end
			
	RRR = 6378.388;
	q1 = cos(coord(2,i) - coord(2,j));
	q2 = cos(coord(1,i) - coord(1,j));
	q3 = cos(coord(1,i) + coord(1,j));
	dist(1:i) = round(RRR * acos(0.5*((1.0+q1)*q2 - (1.0-q1)*q3) ) + 1.0);

end

% acos in inverse of cos

d = sum(dist);

end

