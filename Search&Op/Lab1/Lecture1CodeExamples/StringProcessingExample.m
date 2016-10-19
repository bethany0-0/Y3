
clc
clear all

%% How to use fprintf
namestr = 'Usain Bolt';
medalstr = 'gold';
matchstr = '100m'
timestr = 9.58;

fprintf('\n %s won the %s medal in the %s \n for his time of %5.2f seconds.\n',...
         namestr,medalstr,matchstr,timestr);
     
%% How to use strcmp

DNAstrand = 'AAAGTCTGAC';
test1 = strcmp(DNAstrand,'AAAGTCTGAC')       % compare two strings
test2 = strcmpi(DNAstrand,'aaaGTCTGAC')      % compare two strings but ignore case
test3 = strncmp(DNAstrand,'AAAG',4)          % compare only the first 4 chars of two strings
test4 = strncmpi(DNAstrand,'aaaG',4)         % same as above, but ignore case.


%% How to search 

DNAstrand = 'AAAGTCTGACAAAGTCTGACCTGACAAGTCTGA';
results1 = strfind(DNAstrand,'AAA')


%% How to convert 

str = '4000';
money = str2num(str)



