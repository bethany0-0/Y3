function distMatrix = manhattenDistMatrix( points )

[X,Y]= size(points);
distMatrix = [];

for i = 1 : X
dist = [];   
    for j = 1 : X
      x =  abs(points(i,1) - points(j,1))+abs(points(i,2) - points(j,2));
      dist = [dist, (abs(points(i,1) - points(j,1))+abs(points(i,2) - points(j,2)))];
         
    end
    
    distMatrix = [distMatrix; dist];
end

end

