# CDCProject
A simulation of an ecological network using concurrency

## prerequisites
You need an installation of rebar to successfully fetch dependencies and compile the project.
It is avaiable here: https://github.com/rebar/rebar

Secondly a version of erlang needs to be installed, preferably OTP 20.

## Running the project
All commands are supposed to be executed in the top level directory of the project.
Pay attention that the project folder has the same name as the project.

To run the project you need to fetch all dependencies first.
`rebar get-deps` fetches all necessary dependencies.
Once done, compile the project with `rebar compile`.

Then you can run it using the script `./run_locally.sh`.

To look at the current simulation state open `localhost:8080` on your browser.


_TODO_: add option to run local without server
