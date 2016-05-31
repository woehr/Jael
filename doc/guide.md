# Introduction

Jael is a *domain specific language* for writing reliable, low-level embedded software. By focusing on a specific domain, restrictions can be placed on the software to acheive goals that would be difficult or impossible to acheive in a general purpose programming language. Jael attempts to explore how various features can be incorporated to acheive embedded specific goals. At a high level, these goals include:
- statically eliminating run-time errors
- statically preventing infinite loops, race conditions, and deadlocks
- statically determining the upper bound on memory usage

Jael places an emphasis on indentifying these issues statically. There are several reasons why this is desirable when developing embedded software, for example:
- debugging tools are often less capable than those on desktop systems (often times, low-cost processors will have no on-chip debugging capabilities)
- embedded software may operate remotely making servicing malfunctioning software difficult
- embedded software may be used in situations where software malfunctions can have an extremely high impact (e.g., in medical devices or aeronotics and vehicle components)

# Intended Audience

Jael is not a typical programming language and as such, many of the concepts may appear foreign to existing embedded programmers. The purpose of this document is to introduce Jael to such programmers. Where applicable, features and concepts will be compared to what is typical of low-level embedded systems, namely imperitive languages such as *C*. As such, a familiarity with such languages is assumed.

Concepts and syntax will be introduced sequentially in a way such that the next section depends on the previous one. Therefore, it is recommended that this document be read top to bottom.

# Design

## Sequential Computation

### Types

### Type Inference

### Type Refinements

### Limitations

## Concurrent Computation

### Process Model

### Sessions (Protocols)

### Linearity

## Accessing and Modelling Memory

# Syntax Reference

# References
