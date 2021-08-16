It's really hard to get software right the first time. We might start with a simplified model, an approximation, and slowly work our way towards what we really want. Computers require instruction at such a minute level of detail, we're working with bits, bytes, arrays, stacks, sets, etc. These are hard mathematical things, not so close to the real-world that we want to model. It's a small miracle that we're able to heap abstractions high enough to reach from bits all the way to controlling airplanes, translating sentences, and rendering video games (for just three examples).

The point is, bits are quantized, hard, and idealized. The real world is fuzzy, soft, and [has a surprising amount of detail](http://johnsalvatier.org/blog/2017/reality-has-a-surprising-amount-of-detail). The only way to get anything done is to start small and simple. Slowly add more and more of the details, edge cases, and surprises. You start to asymptotically approach the ideal. Software projects are never done, never perfect. Eventually they peter out or enough other software depends on them that they can't change anymore, or they'll [break something](https://xkcd.com/1172/).

Today I'm interested in software evolution. I want to look at it from a few angles, without addressing any too deeply (today).

## 1. Version control

It's almost hard to believe now, but there was a time, not too long ago, when version control didn't exist and software developers had to, like, re-punch all their punch cards or something.

/*
Version control is one of the most important tools in a software developer's toolbox. No matter the size of your team, it's considered a good idea to track your changes over time. ...

One interesting pattern that version control enables is different versions of some software evolving concurrently. In other words, branches, or more dramatically, forks.
*/

## 2. Language evolution

Like all software, programming languages must evolve (they *are* still software, after all). In fact, updating a language and updating an API have most all the same problems.

## 3. Merge conflicts

## 4. Large-scale updates

