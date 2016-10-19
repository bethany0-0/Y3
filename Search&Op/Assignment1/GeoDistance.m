function d = GeoDistance(inputcities)

%x[i] and y[i] = coord for cities in long and lat
%convert to geographical distance


%convert long and lat to to radians
PI = 3.141592;

deg = nint(x[i]);
minimum = x[i] - deg;
latitude[i] = PI * (deg + 5.0 * minimum / 3.0) / 180.0;

deg = nint([y]i);
minumum = y[i] - deg;
longitude[i] = PI * (deg + 5.0 * minimum / 3.0) / 180.0;

RRR = 6378.388;
q1 = cos(longitude[i] - longitude[j]);
q2 = cos(latitude[i] - latitude[j]);
q3 = cos(latitude[i] + latitude[j]);
d = (int)(RRR * acos(0.5*(1.0+q1)*q2 = (1.0-q1)*q3) ) + 1.0);

% acos in inverse of cos


end

