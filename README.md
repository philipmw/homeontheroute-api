# homeontheroute #

An OTP application providing a public HTTP API that powers [Home On The Route](http://homeontheroute.com), a web app helping you find the perfect neighborhood for your car-free lifestyle.

## Build status

* prod: [![CircleCI](https://circleci.com/gh/philipmw/homeontheroute-api/tree/master.svg?style=svg)](https://circleci.com/gh/philipmw/homeontheroute-api/tree/master)

## API

### Get a list of all stops

*`GET /stops`*

Takes no query params.  Returns JSON: a list of transit stops.

### Get transit score pins

(_Not implemented yet._)

*`GET /transitscore-pins`*

Query params:

* `userLocations[]`: a list of coordinates of the user's favorite locations
* `radiusMeters`: radius, in meters, of each data bin.  This is intended to be synced with the `radius` parameter to `Microsoft.Maps.DataBinningLayer`.
* `originCoords`: coordinates of the bounding box's (0, 0) point
* `bbWidthMeters`: the bounding box's width, in meters
* `bbHeightMeters`: the bounding box's height, in meters

Returns [GeoJSON](http://geojson.org/) of pins, within the specified bounding box, spaced _<radius>_ meters apart, each having a `transit-score` metadata property.

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

Now try http://localhost:8080/stops -- it should work!

## Quit the shell

    1> q().

## Deploying to AWS

### Install tools

    $ brew install awscli
    $ brew install awsebcli

### First time

This section is to be done just once in your AWS account.

1. Update `./aws/stack.yml` to either use a custom domain name (since homeontheroute.com is taken!), or remove
   the `DomainName` resource.
2. Using either the AWS console or CLI, create a new CloudFormation Stack from `./aws/stack.yml`.

This creates a stock Elastic Beanstalk environment, ready for you to deploy this app into it.

### Package for deployment

    $ docker build -t homeontheroute .

### Run Dockerized locally

If you want to test it.  This should behave equivalently to the "Run locally" section.

    $ docker run -p 8080:8080 homeontheroute

## Push to AWS

Follow steps in `./.circleci/config.yml`.  This is what CircleCI does at each commit, so it's
guaranteed to be accurate and up-to-date.

If you are setting up your own CircleCI environment for this project, define these environment
variables for the project:

* AWS_ACCOUNT_ID
* AWS_REGION
