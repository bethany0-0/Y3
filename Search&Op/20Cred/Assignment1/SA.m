function xOut = SA(inputcities)


x = x0;		% Initial solution
e = f(x) 	% Objective function value (energy). 
xbest = x; 	% Initial “best” solution 
k = 0 			% Count evaluation number. 
while (k < kmax)
	T = temperature(t0, k) 	% Temperature calculation. 
	xnew = neighbour(x) 	% Pick some neighbour. 
	enew = f(xnew) 		% Compute its objective function value. 
	
	if P(e, enew, T) > R(0, 1) then 	% Should we move to it? 
		x = xnew; 
		e = enew 			% Yes, change state. 
	if enew < ebest then 			% Is this a new best? 
		xbest = xnew; 
		ebest = enew 			% Save as ’best found’. 
	k = k + 1 				% Increase Evaluation
	end
end

xOut = xbest
end
