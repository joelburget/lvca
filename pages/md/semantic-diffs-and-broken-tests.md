This is a small point complementing the main [semantic diffs](/semantic-diffs) post. It's not quite important enough to roll into the main post, but still worth pointing out.

When structuring a set of changes, two considerations are in tension.

1. The desire to have a clean history. In particular, we would like all tests to pass after every change. Never leave the codebase in a broken state.
2. Showing your work. It's worthwhile to record in granular detail all the steps you took to achieve your goal.

I'd like to justify this second point with a couple of examples.

1. Recording failures and dead ends. In my experience I often try to make some change a few different ways before I find the one that ultimately ends up working. Usually my coworkers think of some of the same ways to accomplish the goal, so the conversation comes up, *why didn't you do it this way*? My explanation is sometimes satisfying, sometimes not. How nice it would be then, if I always had a complete log of my work, so I could point to an exact state of the codebase, *this is where I realized the approach wouldn't work*.
2. Analyzing changes and building tools for common workflows. Another style of change that I see frequently is the *heuristic*, *fixup* pair. I start by applying a heuristic change, for example a simple search and replace. But it's often lossy, with false positives or negatives which I need to fix up by hand. Again, I would like to record this process. Each of these changes would make good fodder for investment in better tools. If I notice the same pattern often enough it's a clue that I should either think of a more accurate heuristic or invest in building a better tool to do the same job.

In both of these cases (especially the second), I want to record a broken state, but I would like this to not affect the cleanliness of the history. Structured diffs suggest a satisfying way to have both. I can simply enforce that all top-level changes maintain passing tests, but sub-changes can break freely.
