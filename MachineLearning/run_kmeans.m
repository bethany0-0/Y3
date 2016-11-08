%%
% based on code from Dr Iain Styles
close all; clear all; clc;
%% get the data ready for clustering
% read the image from file
% im = imread('rgbeye.jpg');
im = imread('MRI.jpg');
% im = imread('horse.jpg');

nR = 128; nC = 128; % specify number of rows and columns
im = imresize(im, [nR, nC]); % reduce size for quick computations
nD = size(im,3);
image(double(im)); axis equal; axis off;
% put it into right shape: one pixel per column
X = double(reshape(im,nR*nC,nD)');
%% create distance function handle
dis = @(p,q) ((p-q)'*(p-q)); % squared Euclidean distance
%% cluster in K clusters
K=3;
[cmean,ClusterIndex]=kmeans_cluster(X,K,dis);
%% display clustering results
im2=reshape(ClusterIndex,nR,nC);
figure; 
subplot(121), imagesc(im)
subplot(122), imagesc(im2); colormap gray; impixelinfo