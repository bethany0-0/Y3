function Griewank=Griewank(x)

Bound=[-600 600];

if nargin==0
    Griewank = Bound;
else
    x=x';
    [Dim, PopSize] = size(x);
    indices = repmat(1:Dim, PopSize, 1);
    Griewank = .00025*sum((x-100).^2) - prod( cos((x-100)./sqrt(indices')) ) +1;
end