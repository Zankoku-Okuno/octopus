OCTOPUS IS DEAD! LONG LIVE AMMONITE!
====================================

The fact is, this implementation is too complex.
Octopus has been plagued by multiple syntax re-designs, huge parser bugs, and even a bit too much purity.
I need a better overall architecture and more thorough design work.
Have no fear, though, the core concepts remain and are being worked on and expanded (well, when I'm not working on my thesis).

UPDATE:
Progress in the replacement language has been coming along smoothly.
So smoothly, in fact, that I am ready to point people to it.
[Ammonite](https://github.com/Zankoku-Okuno/ammonite) is what Octopus should have been from the start.
At the time of this writing, Ammonite has just become Turing-complete, but that's not saying much.

In case you've landed here and don't know what I'm trying to do with Ammonite (formerly Octopus), I've kept some of the old readme below.

Octopus
=======

Octopus is a hacking language. Dynamism and expressivity are the watchwords of Octopus; bonus points for parsimony of definition. These will continue to trump such trivial concerns as reliability or performance. For reliability, exercise taste and discretion; for performance, upgrade your hardware.

Features
========

Executive summary: it's a Lisp with first-class-everything. For more detail, it has:

* Code is data/Data is code: Write programs to write your programs for you; eradicate boilerplate.
* Vau operator: You control when arguments get evaluated; mix and match lazy and strict.
* Immutable values: Data structures are default persistent; never corrupt your program state again.
* Streamlined syntax: With only a handlful of grammar rules, you might miss them if you blink.
* First-class environments: You decide what's in scope and what isn't.
* First-class control: Conquer the last resistance to abstraction; define any control flow construct ever dreamt of.
* Permissive identifiers: Unicode identifiers and user-defined distfixes open new possibilities; make Lisp read/write.
* Natural concurrency: Any concurrency model can be implemented as a library; tame the multiprocessing beast.

Future Features
---------------

These may be on the way, but Octopus is not a huge priority or anything, so we'll see. If I find myself not having fun implementing them, I'll probably just drop them.

* Channel-based concurrency.
* Deliver Octopus to the browser with an interpreter in Javascript.
* Foreign function interface to C code so you can jump out and leverage performance and existing libraries where needed.
* Formal definition.

Never-features
--------------

Good luck to the brave soul who wants to try a static analysis of Octopus, or any language with first-class environments.

That being said, static types are impossible in general, compilation gains you little. When faced with a choice between static and dynamic, Octopus will always choose dynamic, and that makes Octopus a horror story for compiler writers. That's fine, not every language need be compiled.

Status
======

* 2014-03-31: With bugfixes, achieved Turing-complete. The next step is usable.
* 2014-04-06: Multiple files, interpreter program, I/O primitives, exception handling.
              Still on the way to tasty.
* 2014-03-29: Work on Octopus officially halted in favor of [Ammonite](https://github.com/Zankoku-Okuno/ammonite).
 
The Story
=========

Far be it from me to be dogmatic. I'm not usually a dynamic typing guy. I prefer the comfort of a type checker when I have to refactor a system of any size. I like knowing I already have 10x more wiggle room for inefficiency just from compiling to native code. But there comes a time in every programmer's life when they just want to mess around, to build something anxiety-free.

For me, Octopus is that that fun little romp. I started on a Thursday, and had the entire core language by the next Monday. Finishing up the basic primitives and providing an import mechanism is all there is left before it becomes a serious language. Well, and standard libraries... and a package manager... and a community, but you get my point.

