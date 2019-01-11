Philosophy guiding blunt's design
=================================

- Be principled.
- but not opinionated
- Keep the language small. Get the language out of the way.
- Allow the user to build a mental model of the compile process
- Allow the user to hook into the compile process and avoid surprises
- The C machine model is a good abstraction. Do not hide more of the machine
- OOP sucks. It's just a syntax that helps doing certain things a little more
  concisely, but these things are wrong most of the time
- Namespaces suck. The programmer should reference global entities preferably
  using globally unique identifiers. A few more key strokes don't hurt
- Types suck. Programs are too complex to be expressed twice (on the type and
  value levels). Make judicious use of testing instead. Use types only for
  performance (machine layout) and to catch spelling errors
- Simplicity of implementation matters
- Don't aim for performance unless the implementation is straightforward
- Don't add features unless the implementation is obvious
- Compile performance matters
- Minimize surprises
- Don't generalize. Don't build ill-fitting abstractions for the user. Let the
  user build what's required
- Avoid materialized abstractions. It almost never works out
- Do not take shortcuts. Don't be clever. Concision comes from doing the right
  thing. Not from writing the wrong thing in fewer characters
- Control over runtime performance matters
- Micro-benchmarks do not matter
- Plain old data is the answer
