ERLC=/usr/lib/erlang/bin/erlc
ERLCFLAGS=-o
SRCDIR=src
LOGDIR=logs
BEAMDIR=./ebin

all:
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;

	
clean: 
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf erl_crash.dump
