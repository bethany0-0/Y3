function prob = P(e, enew, T)

%acceptance prob function

if enew < e 
	prob = 1;
else
    power = (e-enew)/T;
	prob = exp(power);

end
