# Building a Finite-State-Machine Scala framework

# Conclusion

Let’s have a look at what we built here:

- A way for developers to describe a FSM so it gets compile-time checked by the compiler but use a common algebra for definition. Which is something I didn’t find in the Scala ecosystem.
- A FSM definition focused on the business side of the FSM but a generic abstraction to ease technical features such as telemetry, in a generic way, so developers don’t need to code it every time they develop a new FSM.
- A proper division between declaration and execution, and a modularity in order to achieve a good testability, developer experience and separation of concern.

We were able to answer to both business and technical requirements as stated in the first part but there is still one point I am not very confident with. I am not sure this engine is accessible to all developers. I tried to keep it simple as much as possible but I feel like it can still get a little bit complicated with the long types in the methods (see methods using `State2Algebra`, what about `State4Algebra`).

The next step will be to try this framework at Swan, in production, on a small scope of our payment engine. One of the thing I need to validate is how complicated will be the adoption. My primary goal was to be able to provide a framework that people would understand, and not fear to use. I have no doubt the code execution will work as expected, but I am concerned developers will find the framework too complicated and would like to stay on a more vanilla Scala+Cats code, which I would understand. We would lose the ability to provide tooling over FSM which is something we care about as dev don’t want to bother with telemetry, persistence, and other generic things we do over our programs.

As a personal note, I had fun explore building something generic, with a lot of constraints, revisiting patterns and learning new things.

## Last thoughts

There are a lot of possibilities to build this FSM framework. Over the past months I’ve tried solutions shapeless-based (with Polys, coproduct, hlist). I’ve tried using Free monads, Reader patterns, and other functional programming tools we can use.

I also tried several declaration styles. Constructing the FSM through an initial transition table. Using an implicit resolution at compile time to build the FSM from this table. Each solution having its pros and cons.

In the end, I’ve found Tagless Final to be the sweet spot between the ease of making the framework and the ease of adoption. As I don’t have the knowledge on every pattern, every monad and all programming styles, there still might be room for improvements and I would be happy to take your feedbacks.

### IntelliJ/Metals & Scala 3

I haven’t done much Scala 3 yet and you might find too much of a Scala 2 syntax in my snippets. This project was also a way for me if Scala 3 would bring something different. I am pretty convinced I didn’t use some feature that could have been useful (match types, extension methods, etc). Some of them I didn’t use because the code is understandable without. Some of them because I don’t have the full knowledge about yet. 

However, I’d like to say that IntelliJ is not ready for Scala 3. It has been a pain at every level since there is a lot of implicit resolution in this project. It’s also not correctly typing the methods using match types. I had to run both IntelliJ and VSCode/Metals to make sure I was going the right direction. I’d say that it’s a major issue for the Scala 3 adoption.

## Improvements & ideas

### Better naming

Currently the names are based on what Tagless Final conceptualizes, Algebra, Program & Intrepreter. But this might not be the best terminology if we want all developers to understand what they represent. I have not given too much thoughts about this yet as having a running example was the priority.

### Better algebra DSL

In order to ease the adoption, it should be easier to declare algebras. Having to instantiate the whole trait every time doesn’t make it easy to reason about which kind of algebra we’re making. We could make it more business oriented and provide some helpers (such as building an empty/identity `State0Algebra` or a way to build unitary FSM without needing to specify the `State0Algebra` in the next).

[https://gist.github.com/AlixBa/9037c5d7001a3dec6c8d48f0dadd2b37](https://gist.github.com/AlixBa/9037c5d7001a3dec6c8d48f0dadd2b37)

### Removing Output on State0 & more

I don’t really know if the `output` function on the `State0Algebra` is useful. I had in mind that we might want to map the last state to an output but is this the responsibility of the FSM? Shouldn't it be the caller's responsibility to hide the FSM states & implementation? We could also think about merging `transition` and `exitAction` to a function doing both. Less explicit but do we really need to separate them in the end? 

### Resume or Start with Picker

In the Part 3 I used one way of doing a `ResumeAndStart` state, plugged to another FSM. What I don't particularly like is that you either have to re-build the whole algebra using several merges and it forces you to include it as part of your declaration. Or you combine it with an existing FSM but you still have to merge it with the logic on each state you can resume to. 

To tackle this, we can have a type class to pick a state from an existing algebra. The idea is that you define your business algebra and you should do it only once. This `ResumeAndStart` could be agnostic as it's part of the resilience/idempotency/audit side of the code. Ideally we would want developers to be able to plug it into anything in a generic way. But to do so we need to be able to extract an existing state from an algebra.

[https://gist.github.com/AlixBa/061e758f07c39e929f03d250a257eaa1](https://gist.github.com/AlixBa/061e758f07c39e929f03d250a257eaa1)

**This code doesn’t compile**. We could then try to provide a generic way to have this ResumeOrStart state on top of any FSM using implicit resolution to map outputs to states in an existing FSM.

### More than type aliases

We’ve not defined proper types for `Action`, `Input`, `Output` and `Transition`. We could turn them into proper classes/traits to provide generic & named implementations. If we were to include them into the documentation, it could be good to have some properties to display Action's name for instance.