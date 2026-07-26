---
name: writing-fakes
description: How to draw the trait seam so a fake never re-implements business logic — push logic above the seam into a shared outer class, use default methods, or extract a helper. Use before writing any Fake*/Stub*/InMemory* test double, or when a change to a real implementation forces a parallel change to its fake.
---

# Share business logic between real and fake implementations

When a trait has both a real and a fake/test implementation, draw the
trait so the business logic lives in **shared** code, not duplicated
across implementations. The two should differ only at the
infrastructure boundary — where data is stored, which HTTP backend is
called, what clock ticks — never in their understanding of the rules.

Whenever you reach for a new fake (`FakeFooClient`, `StubFooService`,
`InMemoryFoo…`), first ask whether the logic you're about to copy
belongs above the trait. If the fake needs to re-implement the same
merge rule, the same write-through ordering, the same "don't publish
on no-op" filter, that logic is on the wrong side of the seam.

How to push it up:

- **Split the trait into two layers.** The outer concrete class owns
  the business logic; it depends on an inner trait that's a narrow
  infrastructure boundary. Only the inner trait gets a fake. Example:
  `MovieCache` (concrete — Caffeine, write-through, event publishing)
  depends on `MovieRepository` (trait — Mongo or in-memory). Tests inject
  `InMemoryMovieRepository` and get the real cache semantics for free.
- **Default methods on the trait.** `trait Foo { def primitive(): X;
  final def derived(): Y = ... }`. Real and fake implement primitives;
  derived behaviour is shared by construction.
- **Extract a helper / pure function** both impls call.

Treat the refactor as part of the change that introduces the fake —
not a follow-up. A fake that re-implements logic the real class has is
worse than no fake: it lets tests pass while real code is broken (or
vice versa).

Signs you've drawn the seam in the wrong place:

- The fake has its own copy of a sort/merge/filter rule the real impl
  also has.
- A behaviour change to the real impl forces a parallel change to the
  fake to keep tests green.
- The fake's body is longer than "store this, return that" — it's
  actually deciding things.
- Two tests against the fake disagree about the rule because each
  patched it differently.

Done right, a fake is boring: a `HashMap`, a fixed list of HTTP
responses, a `Clock.fixed(...)`. The business logic sits above and is
exercised end-to-end with the real outer class.
