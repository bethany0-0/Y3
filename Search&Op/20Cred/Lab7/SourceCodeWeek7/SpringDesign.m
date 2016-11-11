function [f_new f G] =SpringDesign(individual)
penalty_factor = 0.1;
Bound=[0.05 2; 0.25 1.3; 2 15];

if nargin==0
    f_new = Bound;
else   
    d = individual(1, 1);
    D = individual(1, 2);
    N = individual(1, 3);
    
    g = zeros(1,10);
    
    g(1) = 1-(D^3*N)/(71785*d^4);
    g(2) = (4*D^2-d*D)/(12566*(D*d^3-d^4))+1/(5108*d^2)-1;
    
    g(3) = 1-140.45*d/(D^2*N);
    g(4) = (D+d)/1.5-1;
    
    % Constraints for bound intervals of decision variables
    g(5) = d - 2;
    g(6) = 0.05 - d;
    g(7) = D - 1.3;
    g(8) = 0.25 - D;
    g(9) = N - 15;
    g(10) = 2- N;
    
    G = max(0, g);
    
    f = (N+2)*D*d^2;
    f_new = f + sum(penalty_factor*G);
end
end

