Philosophy guiding blunt's design
=================================

- Simplicity of compiler implementation matters
- Compile performance matters
- Be principled
- Minimize surprises
- Keep the language small. Get the language out of the way.
- Don't build ill-fitting features for the user. Let the user build what's
  required
- Allow the user to build a mental model of the compile process
- Allow the user to hook into the compile process and avoid surprises
- The C machine model is a good abstraction. Do not hide more of the machine
- Avoid materialized abstractions. It almost never works out
- Do not take shortcuts. Don't be clever. Concision comes from doing the right
  thing
- Control over runtime performance matters
- Micro-benchmarks do not matter
- Don't aim for performance unless the implementation is straightforward
- OOP sucks. It's just a syntax that helps doing certain things a little more
  concisely, but these things are wrong most of the time
- Namespaces suck. The programmer should reference global entities preferably
  using globally unique identifiers. A few more key strokes don't hurt
- Types suck. Programs are too complex to be expressed twice (on the type and
  value levels). Make judicious use of testing instead. Use types only for
  performance (machine layout) and to catch spelling errors
- Plain old data is the answer
