# CDCProject
A simulation of an ecological network using concurrency distributed on a network.

## Prerequisites
You need an installation of rebar to successfully fetch dependencies and compile the project.
It is avaiable here: https://github.com/rebar/rebar  
or for unix users:
`sudo apt-get install rebar`

Secondly a version of erlang needs to be installed, preferably OTP 20, otherwise
new dependencies might not work properly. If you do not have a current version you can
get it with the following commands:   
`sudo apt-get purge erlang` *uninstalling old version*  
`sudo apt-get install esl-erlang=1:20.1` *install new version*  

## Running the project
All commands are supposed to be executed in the top level directory of the project.  
Pay attention that the project folder has the same name as the project.

To run the project you need to fetch all dependencies first.
`rebar get-deps` fetches all necessary dependencies.
Once done, compile the project with `rebar compile`.

Then you can run it using the script `./run_locally.sh`.

To look at the current simulation state open `localhost:8080` on your browser.

## Customization
In config/config.json you can change the gridsize. It is read in by the running-script.  
There is a Makefile included which compiles just the project files without
dependencies.  
To just test the server without the simulation you can run `./test_server.sh` This lets you
examine the interface, but the server will crash when you try to send commands for the
simulation.