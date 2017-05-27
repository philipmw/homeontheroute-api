FROM erlang:latest

RUN mkdir /hotr
COPY . /hotr
RUN rm -rf /hotr/_build
RUN rm -rf /hotr/rebar.lock

RUN cd /hotr && rebar3 eunit
RUN cd /hotr && rebar3 release -n prod

EXPOSE 8080
CMD cd /hotr && ./_build/default/rel/prod/bin/prod foreground
