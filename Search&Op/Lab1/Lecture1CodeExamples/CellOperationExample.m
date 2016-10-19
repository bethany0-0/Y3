fid = fopen('Hamlet.txt');  % Open file
text = textscan(fid,'%s');  % Grab every word and put it in a cell array
fclose(fid);                % Close file

text = lower(text{:});
Nwords = numel(text)

% delete any tokens that do not contain at least one alpha character
% isstrprop Check if string elements are of a specified category.
noAlpha = cellfun(@(x)~any(x),isstrprop(text,'alpha'));
text(noAlpha) = [];

%remove punctuation and any non-alpha characters
puncRemover = @(str)str(isstrprop(str,'alpha'));
text = cellfun(puncRemover,text,'UniformOutput',false);


%find the unique words (in alphabetical order)
% and assign them numeric ids, which is the locn
% of the last occurence of this word in the text.
% wordOrder is a numeric encoding of text using tokens from numericIDs.
[uniqueWords, numericIDs, wordOrder] = unique(text);
numel(uniqueWords)

%count how often each word occurs.
counts = histc(wordOrder,1:numel(uniqueWords));

[frequency,order] = sort(counts,'descend');
%frequency = frequency / sum(frequency);

%list the words from most frequently occurring to least.
sortedWords = uniqueWords(order);

% List 100 most frequent words
sortedWords(1:100)
