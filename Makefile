ERLC=/usr/lib/erlang/bin/erlc
ERLCFLAGS=-o
SRCDIR=src
LOGDIR=logs
BEAMDIR=./ebin


all: 
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
	@ mkdir -p $(LOGDIR) ;
	
clean: 
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf erl_crush.dump
