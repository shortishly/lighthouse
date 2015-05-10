FROM shortishly/erlang
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>

RUN yum -y update && yum -y install \
    git

WORKDIR /opt
RUN git clone https://github.com/rebar/rebar.git
RUN cd rebar && ./bootstrap
RUN cd /bin && ln -s -v /opt/rebar/rebar .

WORKDIR /opt
RUN git clone https://github.com/shortishly/lighthouse.git
WORKDIR lighthouse
RUN git checkout develop
RUN make

ENTRYPOINT _rel/lighthouse/bin/lighthouse console
EXPOSE 8181
