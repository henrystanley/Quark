
## TODO ##

- organize and eliminate redundant import statements

- update api docs

- utf-8 support

- refactor everything (this will probably always be in TODO)

- con and uncon functions

- make @> and <@ throw errors if they try to pop a function from a quote

- figure out a better implementation for variables (hopefully more hygenic and faster)

- change `IState` to be a `ExceptT ErrorMsg IO QVM`

- clean up parser (it's a bit of a mess right now)


## Ideas ##

- int/floor function

- mod function

- regex function

- remove single quote strings so `'` can be used in function names

- getState and setState functions
