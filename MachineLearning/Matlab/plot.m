function [ output_args ] = plot( m1,e1,m2,e2 )
X = mvnrnd(m2,e2,100);
Y = mvnrnd(m2,e2,100);
plot (X(:,1), X(:,2), 'r+', Y(:,1), Y(:,2), 'bo')
end

