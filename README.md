# homeontheroute #

An OTP application providing a public HTTP API that powers [Home On The Route](http://homeontheroute.com), a web app helping you find the perfect neighborhood for your car-free lifestyle.

## Architecture

                    [Application supervisor]
                                |
               /                |
              v                 v
    [Visitor counter]      [Webserver]
                                |
                                |
                                v
                             [Cowboy]

## Add GTFS data

This application needs GTFS data.  (Tested only with King County Metro's datasets.)
Because GTFS data is large, I don't include it in the source repository, so download it yourself and add it to `./priv`.
Download it, unzip it, and update `transit_server` to point to the right directory.

Here's the intended directory structure:

    ebin/...
    priv/metro-gtfs-2016-11-09/stops.txt
    priv/metro-gtfs-2016-11-09/...
    src/...

## Data structures

| ID   | Name                  | Latitude | Longitude |
| ---: | :-------------------- | -------: | --------: |
| 1000 | Pine St & 9th Ave     | 47.613   | -122.332  |
| 3200 | E Union St & 20th Ave | 47.613   | -122.306  |
[Stops (records/stops.hrl)]

| From Stop Id | To Stop Id | Transit Mode | Wait Mins | Travel Mins |
| -----------: | ---------: | :----------- | --------: | ----------: |
| 1000         | 3200       | route 28     | 4         | 20          |
[Direct connections between stops (records/sconn.hrl)]

## Build

    $ rebar3 compile

## Test

    $ rebar3 eunit

## Dialyze

One-time:

    $ dialyzer --build_plt --apps erts kernel stdlib eunit ./_build/default/lib

As needed:

    $ dialyzer --src src

## Run locally

    $ rebar3 shell

And look for this output:

    =PROGRESS REPORT==== 3-Mar-2017::19:08:02 ===
             application: homeontheroute
              started_at: nonode@nohost

This is all you need for complete local development.

## Deploying to AWS

### Install tools

    $ brew install awscli
    $ brew install awsebcli

### Package for deployment

    $ docker build -t homeontheroute .

### Run Dockerized locally

If you want to test it.  This should behave equivalently to the "Run locally" section.

    $ docker run -p 8080:8080 homeontheroute

## Push to AWS

    # ... export AWS credentials ...
    $ aws ecr get-login --region us-west-2
    $ <line from above>
    $ docker build -t homeontheroute .
    $ docker tag homeontheroute:latest 101804781795.dkr.ecr.us-west-2.amazonaws.com/homeontheroute:latest
    $ docker push 101804781795.dkr.ecr.us-west-2.amazonaws.com/homeontheroute:latest
    $ eb deploy