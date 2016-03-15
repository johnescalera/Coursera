function stop = outfun( x, optimvalues, state )
%outfun
%   plot cost
    stop = false;
    hold on;
    plot(optimvalues.iteration,optimvalues.fval,'.');
    drawnow

end

