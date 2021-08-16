My mental image for how software development should work is not unlike how Github works today, except TODO describe how:
* it's easy to fork and stuff, like more real-time
* You can query over the whole universe

But there's one big problem with working this way that I've never been able to resolve -- licenses. If I'm writing MIT-licensed code I should never rely on GPL code. It's a real mess if everybody is working in the same big sandbox. But today I realized that we can solve the problem by having separate *universes* (maybe think of a better name), where GPL and MIT live in separate universes. With some licenses it's okay to depend on others, with some it's not. The set of universes forms a DAG by the okay-to-include relation.
