--Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
--How many such routes are there through a 20x20 grid?

-- There are 20 horizontal and 20 vertical routes
-- so we use permutations to "choose" H and V routes.
-- prob15 = 40 P 20 / 20 P 1
prob15 = product [21..40] `div` product [2..20]