function [theta, J_history] = gradientDescent(X, y, theta, alpha, num_iters)
%GRADIENTDESCENT Performs gradient descent to learn theta
%   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 1);

for iter = 1:num_iters

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    %

    v_predictions = X * theta; 
    v_error = (v_predictions - y);

    theta_t0 = theta(1) - alpha * (1/m) * sum(v_error .* X(:,1));
    theta_t1 = theta(2) - alpha * (1/m) * sum(v_error .* X(:,2));

    theta = [theta_t0; theta_t1];



    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCost(X, y, theta);

end

   % J_history = [(1:length(J_history))', J_history]; % add index to vector
   % plot(J_history(:,1), J_history(:,2));

end
