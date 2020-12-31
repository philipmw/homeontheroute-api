# homeontheroute #

An OTP application providing a public HTTP API that powers [Home On The Route](http://homeontheroute.com), a web app helping you find the perfect neighborhood for your car-free lifestyle.

Home On The Route offers _personalized_ Transit Scores for your area.  [The Trouble with "Transit Score".](http://humantransit.org/2017/03/the-trouble-with-transit-score.html)

## Project status

The project is currently a work in progress and is not ready for use.
It is in intermittent development in my free time, as a hobby project.

## Development

### Architecture

There are two Erlang applications here: `hotr_gtfs` and `hotr_web`.
The GTFS application has stateless logic for processing GTFS data,
while the Web application is an OTP server for processing web requests.

### Add GTFS data

This application needs GTFS data.  (Tested only with King County Metro's datasets.)
Because GTFS data is large, I don't include it in the source repository, so download it yourself.
The latest King County Metro GTFS dataset is downloaded as part of this project's continuous deployment,
so refer to `./.circleci/config.yml` for the details.

### Install development software

I develop on macOS with [IntelliJ IDEA](https://www.jetbrains.com/idea/) and
the Erlang plugin.

Use Homebrew to install additional tools:

    $ brew install rebar3

### Build

    $ rebar3 compile

### Test

    $ rebar3 eunit

## Dialyze

One-time:

    $ dialyzer --build_plt --apps erts kernel stdlib eunit ./_build/default/lib

As needed:

    $ dialyzer --src src

## Run locally

    $ rebar3 shell

And look for this output:

    ===> Booted hotr_gtfs
    ===> Booted hotr_web

If it fails to start: did you add your own GTFS data?

This is all you need for complete local development.

Now in the shell you can run something like:

    TransitTable = ets:whereis(transit_data_top).
    trip:optimal_trip_to_stop({trip_config, TransitTable, <<"26860">>, 1}, [{0, walk, 1, <<"18455">>}]).

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
