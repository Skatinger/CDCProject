#!/bin/bash


erl -pa ebin deps/**/ebin -eval "master:start_server()."
