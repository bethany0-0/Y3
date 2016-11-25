function distMatrix = eucledianDistMatrix( points )

[X,Y]= size(points);
distMatrix = [];

for i = 1 : X
dist = [];   
    for j = 1 : X
    %  x =  (points(i,:) - points(j,:))*(points(i,:) - points(j,:))';
      dist = [dist, (points(i,:) - points(j,:))*(points(i,:) - points(j,:))'];
         
    end
    
    distMatrix = [distMatrix; dist];
end

end

