function [cmean,ClusterIndex]=kmeans_cluster(data,nClusters,dfun)

% based on code from Dr Iain Styles

npts = size(data,2);

% Initialise data structures
cmean = cell(nClusters,1);
newcmean = cell(nClusters,1);
s = zeros(nClusters,npts);

% Randomly assign data into initial clusters
ClusterIndex = randi(nClusters,1,npts);
% Compute the cluster means
for ii=1:nClusters
  cmean{ii} = mean(data(:,ClusterIndex==ii),2);
end

Converged = false;
while(~Converged)
    % Compute the distance from each data points to each of the means
    for ii=1:nClusters
        for ij=1:npts
            s(ii,ij) = dfun(data(:,ij),cmean{ii});
        end
    end
    % Find the closest means
    [~,NewClusterIndex] = min(s,[],1);
    % Compute the new clusters means
    for ii=1:nClusters
        newcmean{ii} = mean(data(:,NewClusterIndex==ii),2);
    end
    % Check to see if the clusters have changed
    if NewClusterIndex == ClusterIndex
        Converged = true;
    end
% Assign new values to the clusters
ClusterIndex = NewClusterIndex;
cmean = newcmean;
end