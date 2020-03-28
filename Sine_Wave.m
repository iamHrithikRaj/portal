clc;
clear all;
Fs = 100;             % Sampling frequency (samples per second) 
dt = 1/Fs;            % seconds per sample 
StopTime = 1;         % seconds 
t = (0:dt:StopTime)'; % seconds 
F = 2;                % Sine wave frequency (hertz) 
rng = 2*pi*F*t;

nf = @(t,h) sin(t).*(sin(t)>=h) + h*(sin(t)<h);  % Clipper Sine Function (-ve)
pf = @(t,h) sin(t).*(sin(t)<=h) + h*(sin(t)>h);  % Clipper Sine Function (+ve)

thres = input('Enter Threshold: ');
if thres>=0
    hold on
    plot(t,sin(rng),'b--')   %uncomment to compare
    plot(t, pf(rng,thres))
else
    hold on
    plot(t,sin(rng),'b--')  %uncomment to compare
    plot(t,nf(rng,thres))
end
xlabel('Time(s)');
grid
axis([0  1   -1  1])    