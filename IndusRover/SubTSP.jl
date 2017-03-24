using JuMP
# using Gurobi
using GLPK
using GLPKMathProgInterface


# Given a n-by-n matrix representing the solution to an undirected TSP,
# extract the tour as a vector
# Input:
#  n        Number of cities
#  sol      n-by-n 0-1 symmetric matrix representing solution
# Output:
#  tour     n+1 length vector of tour, starting and ending at 1
function extractTour(n, sol,origin)
    tour = [origin]  # Start at city 1 always
    cur_city = origin
    while true
        # Look for first arc out of current city
        for j = 1:n
            if sol[cur_city,j] >= 1-1e-6
                # Found next city
                push!(tour, j)
                # Don't ever use this arc again
                sol[cur_city, j] = 0.0
                sol[j, cur_city] = 0.0
                # Move to next city
                cur_city = j
                break
            end
        end
        # If we have come back to 1, stop
        if cur_city == origin
            break
        end
    end  # end while
    return tour
end

function findSubtour(n, sol)
    # Initialize to no subtour
    subtour = fill(false,n)
    # Always start looking at city 1
    cur_city = 1
    subtour[cur_city] = true
    subtour_length = 1
    while true
        # Find next node that we haven't yet visited
        found_city = false
        for j = 1:n
            if !subtour[j]
                if sol[cur_city, j] >= 1 - 1e-6
                    # Arc to unvisited city, follow it
                    cur_city = j
                    subtour[j] = true
                    found_city = true
                    subtour_length += 1
                    break  # Move on to next city
                end
            end
        end
        if !found_city
            # We are done
            break
        end
    end
    return subtour, subtour_length
end


# solveTSP
# Given a matrix of city locations, solve the TSP
# Inputs:
#   n       Number of cities
#   cities  n-by-2 matrix of (x,y) city locations
# Output:
#   path    Vector with order to cities are visited in
function solveTSPsub(n, dist)

    # Calculate pairwise distance matrix
    
    # Create a model that will use Gurobi to solve
    m = Model(solver=GLPKSolverMIP())

    # x[i,j] is 1 iff we travel between i and j, 0 otherwise
    # Although we define all n^2 variables, we will only use
    # the upper triangle
    @variable(m, x[1:n,1:n], Bin)

    # Minimize length of tour
    @objective(m, Min, sum(dist[i,j]*x[i,j] for i=1:n for j=i:n))

    # Make x_ij and x_ji be the same thing (undirectional)
    # Don't allow self-arcs
    for i = 1:n
        @constraint(m, x[i,i] == 0)
        for j = (i+1):n
            @constraint(m, x[i,j] == x[j,i])
        end
    end

    # We must enter and leave every city once and only once
    for i = 1:n
        @constraint(m, sum(x[i,j] for j=1:n) == 2)
    end

    
    solve(m)

    # Return best tour
    # return extractTour(n, getvalue(x))
    return x
end  # end solveTSP

function solveTSP(n, dist)


    # Create a model that will use Gurobi to solve
    m = Model(solver=GLPKSolverMIP())

    # x[i,j] is 1 iff we travel between i and j, 0 otherwise
    # Although we define all n^2 variables, we will only use
    # the upper triangle
    @variable(m, x[1:n,1:n], Bin)

    # Minimize length of tour
    @objective(m, Min, sum(dist[i,j]*x[i,j] for i=1:n for j=i:n))

    # Make x_ij and x_ji be the same thing (undirectional)
    # Don't allow self-arcs
    for i = 1:n
        @constraint(m, x[i,i] == 0)
        for j = (i+1):n
            @constraint(m, x[i,j] == x[j,i])
        end
    end

    # We must enter and leave every city once and only once
    for i = 1:n
        @constraint(m, sum(x[i,j] for j=1:n) == 2)
    end

    function subtour(cb)
        # Optional: display tour starting at city 1
        println("----\nInside subtour callback")
        println("Current tour starting at city 1:")
        print(extractTour(n, getvalue(x),1))

        # Find any set of cities in a subtour
        subtour, subtour_length = findSubtour(n, getvalue(x))

        if subtour_length == n
            # This "subtour" is actually all cities, so we are done
            println("Solution visits all cities")
            println("----")
            return
        end

        # Subtour found - add lazy constraint
        # We will build it up piece-by-piece
        arcs_from_subtour = zero(AffExpr)

        for i = 1:n
            if !subtour[i]
                # If this city isn't in subtour, skip it
                continue
            end
            # Want to include all arcs from this city, which is in
            # the subtour, to all cities not in the subtour
            for j = 1:n
                if i == j
                    # Self-arc
                    continue
                elseif subtour[j]
                    # Both ends in same subtour
                    continue
                else
                    # j isn't in subtour
                    arcs_from_subtour += x[i,j]
                end
            end
        end

        # Add the new subtour elimination constraint we built
        println("Adding subtour elimination cut")
        println("----")
        @lazyconstraint(cb, arcs_from_subtour >= 2)
    end  # End function subtour

    # Solve the problem with our cut generator
    addlazycallback(m, subtour)
    solve(m)

    # Return best tour
    return x
end  # end solveTSP

function solveTSPSub(n, dist)

    
    # Create a model that will use Gurobi to solve
    m = Model(solver=GLPKSolverMIP())

    # x[i,j] is 1 iff we travel between i and j, 0 otherwise
    # Although we define all n^2 variables, we will only use
    # the upper triangle
    @variable(m, x[1:n,1:n], Bin)

    # Minimize length of tour
    @objective(m, Min, sum(dist[i,j]*x[i,j] for i=1:n for j=i:n))

    # Make x_ij and x_ji be the same thing (undirectional)
    # Don't allow self-arcs
    for i = 1:n
        @constraint(m, x[i,i] == 0)
        for j = (i+1):n
            @constraint(m, x[i,j] == x[j,i])
        end
    end

    # We must enter and leave every city once and only once
    for i = 1:n
        @constraint(m, sum(x[i,j] for j=1:n) == 2)
    end

    
    solve(m)

    # Return best tour
    # return extractTour(n, getvalue(x))
    return x
end  # end solveTSP

function reward2weight(rw)
    cost = (8-rw)
    return cost
end
function getRandomW(df,k)
    W = 100000000+zeros(k,k);
    for ii=1:k
        for jj=1:k
            rw = df[ii,jj]
            if !isna(rw)
                #W[ii,jj] = reward2weight(rw)
                W[ii,jj]=5*rand()
                W[jj,ii]=W[ii,jj]
            end
        end
    end
    return W
end

using DataFrames
df = convert(DataFrame,readtable("AdjReward.csv",header=false))
k=75
open("TourSampling-03.csv","w") do file
    for ii=1:1000
        W = getRandomW(df,k)
        tour = solveTSP(75, W)
        println("Solution: ")
        soln = extractTour(75, getvalue(tour),38)
        println(extractTour(75, getvalue(tour),38))
        write(file,"$soln\n")
    end
end