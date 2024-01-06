# LSH Language

LSH stands for Lisp Shell, which means LSH aims to be a Lisp used in a shell environment.

## Error Stack

There are two kinds of errors in LSH: `ParsingError` and `RuntimeError`. They can't happen at the same time, so if there's a `ParsingError` no code is executed, so no `RuntimeError`, also, if there's a `RuntimeError` it means that no `ParsingError` happened.

### ParsingError

ParsingErrors can be:

- UnknownIdentifier

### RuntimeError

RuntimeErros can be:

- UndefinedIdentifier: An identifier was used but it's not defined in the scope
- NotCallable: Tried to call over a thing that isn't a function nor a macro
- TooManyArguments: Tried to call a function with more arguments than the function requires
- IncompatibleArgument: The function expected an argument of a given type, but other type was provided
- MutatingImmutable: Tried to mutate an immutable value
- GenericError: another runtime error that is thrown by the function, but isn't one of the above

Every error must contain:

- Description: a proper description of what happened and possible solutions if appliable
- Where: The location of where the error happened
- Callstack: The callstack of what threw the error (if not at the top of the callstack)
