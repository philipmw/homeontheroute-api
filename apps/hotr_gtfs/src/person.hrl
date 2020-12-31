% I use this as a way to select eligible first stops.
% It's less for the person's preference and more a way to reduce search space.
-define(MAX_WALK_SECS_TO_NEXT_STOP, 60*10).

% Max walking over the entire trip, not necessarily contiguously.
-define(MAX_WALK_SECS_TOTAL, 60*20).

% https://en.wikipedia.org/wiki/Preferred_walking_speed
% moderated for intersections, stop lights, hills, etc.
-define(WALK_METERS_PER_SEC, 1.0).
