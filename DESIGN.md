# Backend design #

The primary responsibility of the backend is to take a set of desired locations and then to compute, for any arbitrary location, its desirability.

I am using the [data binning layer](https://www.bing.com/api/maps/sdkrelease/mapcontrol/isdk#gradientColorScaleBinning+TS) of Bing Maps SDK to render the desirability of every square of Seattle, so the service needs to return a grid of coordinates and their desirability scores.

## How to calculate desirability score

The desirability score of an arbitrary location _L_ is measured by how long it takes to get from _L_ to all the desired locations.
(Later, we'll need to adjust this definition to include the reverse trip, since the two are asymmetric.)

For example, suppose the user gives us three desired locations: _{A, B, C}_.
Now we want to compute the desirability score of _L_.
To do that, we compute travel time from _L_ to _A_, _L_ to _B_, and _L_ to _C_, then reduce the three travel times to one score.
The higher the travel time, the lower the score.

## How to find travel time

We have the King County Metro GTFS data.
This gives the set of bus routes and their trip frequencies.
With the GTFS data, we can find travel time between two arbitrary transit stops.

At all times, our simulated person can either take transit or walk, depending on which is faster.

Travel time between two locations becomes:

1. Walk either the whole way, or walk to the optimal transit stop.
2. Wait for the optimal bus / train route.
3. Ride either the whole way, or for an optimal amount of time to switch to another route.
4. Repeat steps 2--3 as many times as necessary.  (Transfers.)
5. Walk from the last transit stop to the destination.

Factors to consider:

1. The closest transit stop may not be the optimal stop.  (A farther transit stop may have a more efficient route for your trip.)
2. A person is willing to walk only so far.
3. Often, multiple routes serve the same set of stops.  We prefer this because the person needs to wait less time to board.

Let's get more specific.
Here's some ASCII art.

           SA >--[28]--> SI >----[28]----> SY
    {start}                                           {dest}
               SB >--[40, 48]--> SJ >--[40,48]--> SZ

In this diagram, we want to travel from _{start}_ to _{dest}_.
Transit stops are marked _S*_, with their bus route numbers in square brackets.
Stops _SA_ and _SB_ are both within walking distance from _{start}_.
For example, on stop _SA_ you can board bus route 28 and travel to stop _SI_, then continue on the same bus to stop _SY_.

What's the optimal travel time?

The person has three options to start:

1. Walk directly from _{start}_ to _{dest}_, ignoring transit altogether.
2. Walk to _SA_ (closer) and wait for the 28 to travel to _SI_.
3. Walk to _SB_ (farther) and wait for the 40 or 48 to travel to _SJ_.

Then, at each transit stop they pass, they have another choice to make:

1. Continue on the same bus to the next stop.
2. Disembark and wait for another route at the same stop.  This adds wait time.
3. Disembark and walk to another stop within walking distance.  This adds walking and wait time.
4. Disembark and walk to the destination.  This adds walking time.

Our algorithm needs to evaluate all possible choices the traveler can make.

We cannot use a greedy algorithm here, because locally optimal decisions (such as walking to the closest bus stop as opposed to farther ones) do not necessarily lead to globally optimal outcomes.
This is demonstrated in the diagram above; a farther stop (_SB_) has more routes that serve the destination, thus minimizing wait time.  If the extra time it takes to walk to _SB_ is less than the extra time it takes to wait for 28 rather than for 40/48, then it pays to walk longer to _SB_.

## Route bundles

In the diagram above, the traveler can take either the 40 or the 48 to reach their destination.
Suppose each bus comes every 10 minutes.
If we commit the traveler to one of the two routes, then their average wait time at the stop is 5 mins.
But if we allow the traveler to wait for _either_ 40 or 48, their average wait time drops to 2.5 mins.
(This assumes that the two routes are evenly offset from each other in their arrival times.)

I call this a "route bundle": instead of traveling on the 40, the traveler is traveling on the bundle of 40+48.
Routes within the bundle are equivalent, and the bundle is used to minimize wait time.

This feature is not implemented currently; it is in the backlog.