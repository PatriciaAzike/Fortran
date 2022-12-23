close all; clc; clear all

A=load('aoutput.txt');
figure(1)
for i=1:size(A,1)
    
    plot(A(i,:), 'Linewidth',2); hold on
    
end
xlabel('x'); ylabel('U')
title("Linear Advection Equation")
hold off
