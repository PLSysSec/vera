# Lejit

A DSL and helper framework for range analysis for JavaScript JIT compilers.

Code tour:

src/DSL/ is the directory where a DSL that makes it easier to generate Boolector
constraints lives. BoolectorWrapper.hs wraps some tricky Btor operations, while
DSL.hs provides the bare-bones DSL.

src/IonMonkey contains IonMonkey-specific objects and operations.
In Objects.hs, you will find our representation of the IonMonkey Range object, as
well as operations that make it easier to create ranges and verify their properties.
Operations.hs contains the actual optimizations that we want to verify. Right now,
just starting with int32 operations (our bindings don't support FP).

test/IonMonkey contains the verification code for the operations in IonMonkey/Operations.



