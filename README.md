# Ramjet

A tensor library using scala 3 inlines and literal types to provide a great user experience.

*Make illegal states unrepresentable - Yaron Minsky*

## Features

Ramjet uses type level bounds to ensure all the matrices and vectors you use in your computations have compatible format.
Compilation errors are well formatted and give the user an immediate actionable feedback, without needing to run the program.

Ramjet features polymorphic backends. The `ramjet-core` module provides a very simple Array-backed implementation, 
but implementing your own to support your preferred tensor manipulation lib is quite straightforward.

```scala 3
import ramjet.*

import ArrayBackend.api.* // pull in the typesafe dsl in scope

// copy the provided array and runtime-check the format
// note the resulting type featuring the shape component
val ones: T2[2, 10] = unsafe(2, 10)(Array.fill(20)(1f))

// you also get convienience methods
val ones: T2[2, 10] = fill(2, 10)(1f)

// You can also get back to the underlying tensor
val floats = ones.unwrap.data

// Vectors are also supported
val v: T1[4] = unsafe(4)(Array(1f, 2f, 3f, 4f))
```

### Safe matrix multiplication

```scala 3
val A = fill(2, 10)(1f)
val B = fill(10, 20)(1f)

// operands are compatible, so this compiles
val C = A * B // type is T2[2, 20]

// this does not
val wrong = fill(15, 20)(1f)

val oops = A * wrong // Compile error
// Wrong tensor format: [2, 10] x [15, 20]
```

### Safe reshape

All tensors can be reshaped using type level proofs.
```scala 3
val A = fill(2, 10)(1f)
val B = A.reshape(4, 5) // this is fine

val oops = A.reshape(5, 5) // Compile error
// Error reshaping [2, 10] to [5, 5]
```
