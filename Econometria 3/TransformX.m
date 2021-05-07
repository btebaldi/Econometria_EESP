function [y] = TransformX(x, timecode)
    %    Transform x
    %    Return Series with same dimension and corresponding dates
    %    Missing values where not calculated
    %    -- Tcodes:
    %             1 Level
    %             2 First Difference
    %             3 Second Difference
    %             4 Log-Level
    %             5 Log-First-Difference
    %             6 Log-Second-Difference
    %            16 Log-Second-Difference
    %            17 (1-L)(1-L^12)
    
    %  Translated from the Gauss procs of Stock&Watson(2005),'Implications of
    %  dynamic factor models for VAR analysis'
    %  Dimitris Korobilis, June 2007
    
    small=1.0e-06;
    n=size(x,1);
    y=zeros(n,1);        %storage space for y
    
    switch timecode
        case 1 % Level
            y=x;
        case 2 % First Difference
            y(2:n)=x(2:n)-x(1:n-1);
        case 3 % Second Difference
            y(3:n)=x(3:n)-2*x(2:n-1)+x(1:n-2);
        case 4 % Log-Level
            if min(x) < small
                y=NaN;
            end
            x=log(x);
            y=x;
        case 5 % Log-First-Difference
            if min(x) < small
                y=NaN;
            end
            x=log(x);
            y(2:n)=x(2:n)-x(1:n-1);
        case 6 % Log-Second-Difference
            if min(x) < small
                y=NaN;
            end
            x=log(x);
            y(3:n)=x(3:n)-2*x(2:n-1)+x(1:n-2);
        case 16 % Log-Second-Difference
            if min(x) < small
                y=NaN;
            end
            x=log(x);
            y(3:n)=x(3:n)-2*x(2:n-1)+x(1:n-2);
        case 17 % (1-L)(1-L^12)
            if min(x) < small
                y=NaN;
            end
            x=log(x);
            y(14:n)=x(14:n)-x(13:n-1)-x(2:n-12)+x(1:n-13);
        otherwise
            y=NaN;
    end
    
    
end