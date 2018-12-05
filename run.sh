#!/bin/bash

cd ebin
erl -pa ebin ../deps/*/ebin -eval "application:start(cdcproject)" -eval "init:stop()"

