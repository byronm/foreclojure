# Foreclojure
This repository contains my 4Clojure solutions.

## What is 4Clojure?
4Clojure is an interactive website with a collection of small programming
problems useful for learning Clojure. It's an awesome resource for anyone
looking to learn the basics, improve their skills, or just have tons of fun
writing Clojure.

The original site (hosted at https://www.4clojure.com) was [recently shut
down](https://groups.google.com/g/clojure/c/ZWmDEzvn-Js?pli=1). Thankfully,
others are taking up the mantle. The current best replacement appears to be
https://4clojure.oxal.org.

Thank you to everyone who has contributed to this awesome project over the
years!

## Solutions
I put all of my solutions in a single file so that they're easy to peruse. I
also put the test cases right after each solution (in a `(comment ...)` block)
because a lot of the problems are easiest to grok by just reading their test
cases.

This self-contained approach lets this single file serve as a workbench of
sorts. You can just load the file into your REPL and start tinkering. My
workflow is to work on a solution, send it over to the REPL, and then send each
of the test cases over. Iterate and repeat until they all pass. This super tight
feedback loop is a powerful tool for thought that enables an exhilarating kind
of exploratory programming. How to set this up will depend upon your
[editor](https://clojure.org/community/tools), but the key ingredient is to
reach a point where you can send individual s-expressions over to your REPL,
from your text editor, without copy-pasting.

I strive to make my solutions correct, clear, and concise. It's common to use
anonymous functions `#(...)` in 4Clojure solutions, but I tended towards naming
my functions and locals for readability.

It's helpful to compare your own solutions to everyone else's on 4Clojure; I
learned a ton doing so.

Enjoy!
