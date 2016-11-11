function [ output_args ] = StochasticRanking( Pt )

%what is input, what is penalty function

lambda = size(input);

I = [1:1:lambda];

for i = 1 : N
    
    swap = 0;
    
    for j = 1 : (lambda-1)
        
        %sample u in U(0,1)
        u = rand(1);
        
        if (penalty(j) == 0 && penalty(j) == penalty(j+1)) || (u<Pf)
            if (f(I(j)) > f(I(j+1)))

                swap(I(j), I(j+1)); 
                swap = 1;
            end
        else
            if penalty(j) > penalty(j+1)
                swap(I, j,(j+1)); 
                swap=1;
            end
        end
        
        if swap == 0
            break;
        end
    end
end
    
    
    
end

