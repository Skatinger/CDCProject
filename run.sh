cd ebin
erl -pa ebin -eval "master:start(5,5,5,5)" -eval "init:stop()"

