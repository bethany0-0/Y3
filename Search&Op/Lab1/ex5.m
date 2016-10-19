function value = forLoop(A, B)

	[X,Y] = size(A)
	[C,D] = size(B)
	Final = zeros[X,D]

	if Y = C
		for i = 1:X
			for j = 1:D

				sum = 0

				for k = 1:Y
					sum = sum + (A(i,k) * B(k,j))
				endfor

				Final(i, j) = sum
			endfor
		endfor

	value = Final
end
