# Ramjet

A tensor library using scala 3 inlines and literal types to provide a great user experience.

*Make illegal states unrepresentable - Yaron Minsky*

## Features

Ramjet uses type level bounds to ensure all the matrices and vectors you use in your computations have compatible format.
Compilation errors are well formatted and give the user an immediate actionable feedback, without needing to run the program.

Ramjet features polymorphic backends. The `ramjet-core` module provides only the API.
A backend using the [storch](https://github.com/sbrunk/storch) library is available in the `ramjet-backend-storch` module. 

```scala 3
import ramjet.*

import StorchBackend.api.* // pull in the typesafe dsl in scope

// copy the provided array and runtime-check the format
// note the resulting type featuring the shape component
val ones: T2[Float, 2, 10] = unsafe(2, 10)(Array.fill(20)(1f))

// you also get convienience methods
val ones: T2[Float, 2, 10] = fill(2, 10)(1f)

// You can also get back to the underlying tensor
val floats = StorchBackend.floatArray(ones.unwrap)

// Vectors are also supported
val v: T1[Float, 4] = unsafe(4)(Array(1f, 2f, 3f, 4f))
```

### Safe matrix multiplication

```scala 3
val A = fill(2, 10)(1f)
val B = fill(10, 20)(1f)

// operands are compatible, so this compiles
val C = A * B // type is T2[Float, 2, 20]

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

### Safe append

Concatenating matrices and vector or dimentions is supported :

```scala 3
val A = fill(2, 10)(1f)

val B: T2[Float, 4, 10] = A.append(A, 0) // append on the first axis, rows
val C: T2[Float, 2, 20] = A.append(A, 1) // append on the second axis, columns

// Vectors are also supported

val V = fill(10)(1f)

val D: T2[3, 10] = A.append(V, 0) // another row is appended

// Everthing is typesafe !

val oops = B.append(C, 0) // Compile error, the format are not compatible

```



