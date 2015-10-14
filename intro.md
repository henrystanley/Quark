
A Brief (and/or long winded) Introduction to Quark
==================================================

Hello potential Quark user, before this guide can begin I'd like to run several health warnings by you.

    Do you experience allergic reactions to brackets?
    Does excessive stack manipulation make you break into a cold sweat?
    Are you addicted to Lisp and/or Lisp like substances?

If you meet any of these criteria, please close this txt at once.
With that out of the way, let's continue...

Quark is a simple functional language, oriented towards feature bootstrapping.
Quark like most other concatenative languages is based on a stack model.
Like Joy or Factor, Quark uses quoted lists of functions as it's main method of abstraction.
Unlike Joy or Factor, these quotes can do simple pattern matching on the stack.
This can greatly simplify Quark code in many instances.
Quark doesn't try to be safe, fast, or readable.
It does however, try to be as simple, flexible, and obvious as possible.


Making REPLs in the pond
------------------------

Assuming you managed to install Quark onto your IBM PC, PDP-11, or MIT Lisp Machine, let's fire up the repl.
Quark has two modes of operation, interpreter and repl.
To interpret a file, give `quark file-to-run.qrk` a try.
To start the repl just type `quark` without any arguments.
If you managed to get this far you should see a prompt that looks like this:

    :>

Let's begin our examination of the language by looking at the data types Quark provides.
Why don't we start by pushing some numbers to the stack?

    :> 42 6.0 -0.976

There we are...
Let's check to make sure that worked.
Here, I'll introduce you to your first quark function: `.`
There are 22 primitive functions in Quark, and this guide should cover them all.
The dot function will print the entire stack, and is mainly used for debugging.

    :> .
    42.0 6.0 -0.976

Numbers in Quark are all Doubles.
This simplifies things, and hopefully provides enough accuracy for most stuff.

Now lets add some strings:

    :> "quarklang" 'single quotes work too...' 'chunky bacon'

Strings can be written with either single or double quotes.
I should point out here as well, that quark doesn't really care about line breaks.
Strings can span multiple lines if necessary, however the repl doesn't really allow for this.
Here's a new, slightly more exotic, type:

    :> :thing :Cows :the_color_blue

These are symbols, and if you're at all familiar with Ruby you should recognize them.
Symbols are literals used for names, keys, and other stuff.
Moving on, let's get acquainted with variables.

    :> x Y potato

Variables are a bit more complicated than the previous types.
Their interpretation depends on the current bindings.
If they are unbound when pushed to the stack they remain as literals.
If however, they are bound to a quote (which we'll explore next) they will call this quote.

Now might be good time (if not a little late) to explain Quark's evaluation model.
The basic haskell Quark implementation represents the state of the environment with a tuple of three things:
Stack, Tokens, and Lib.
Stack is obviously the data stack we've been pushing to this whole time.
Tokens is a list of unevaluated Quark items.
Lib is a simple map of Quark variables to Quark quotes.
Each step of the evaluation the interpreter pops the first item off the Tokens list.
If this is a data item, the interpreter pushes it to the stack.
Otherwise if it is a bound variable it calls the corresponding quote.

Why don't we take a quick peek at these quotes?

    :> [ 4 'fifty' :orange ]

This is a simple quote of three items.
When called it will append these items in order to the Tokens list.
By in order, I should specify, I mean it will concat them like this in pseudo-haskell: [ 4, "fifty", :orange] ++ tokens
Next of course, the interpreter will pop each of these off the tokens list and push them to the stack.
However, since we haven't called this quote, it will just sit at top of the stack, looking sad and forlorned.


Pattern Matching, because if statements are for chumps...
---------------------------------------------------------

Quotes have one more property which is responsible for a lot of Quark's flavor.
Anyone who has used a functional programming language like Erlang, Haskell, or OCaml is probably familiar with pattern matching.
Before I can ramble anymore, let's look at an example of this.

    :> [ 4 | :is_four ]

This rather useless quote consists of a pattern of 4 separated by a '|' character from it's value of :is_four
When a pattern equipped quote is applied, it completes 2.5 steps:

  1. Pop n items off the stack, where n is the length of the pattern (in the case of an empty pattern this is zero)
  2. Check if these popped items are equal to the items in the pattern (if the pattern item is a variable any value will match)
  3 a. If they don't match push the symbol :nil to the stack
  3 b. If they do, replace bound variables in the quote body and call the quote as explained above  

Let's try this out, but before we do allow me to introduce you to two new functions: `print` and `call`
`print` takes a string off the stack and prints it, big surprise right...
`call` takes a quote off the stack and calls it, also fairly obvious.
Now, on to the example:

  :> 6 [ 5 | 'This is a 5' print ] call

This will (presuming it isn't opposite day) *not* print 'This is a 5', instead as noted above it will put :nil on your stack.
Now let's do this:

    :> 'This will print!' [ x | x print ] call

Hopefully you can see what just happened:
  1. We popped the string off the stack
  2. Matched it with (and bound it to) the variable x
  3. Replaced the x in the body with 'This will print!'
  4. Appended the quote body to the Token queue

This should have printed 'This will print!'.
Let's do another!

    :> :cow 'pig' [ :cow other | 'There is a cow and a ' print other print ] call

Like magic, this quote will print if it is applied to a stack with :cow and another arbitrary value.
As a side note, I should point out that matching doesn't recursively check data.
That means, `[ [ a b ] | a ]`, won't do what you are probably expecting if you have a Haskell background.
As another example of my point, this expression:

    :> [ :cow :pig ] [ [ :cow a ] | 'there's a cow and something else' ]

will put :nil on your stack because it only matches literal quote values with :cow and a variable named `a`


I'm a bit def, you'll have to speak up
--------------------------------------

If you've been following these examples your data stack is probably filled with rubbish, let's do something about that...

To start, here's a new function: `def`
`def`, expects a stack of a quote and a symbol, like this:

    :> [ 'apple' ] :push_apple def

`def`, will then create a binding between the variable version of the symbol (in this case: push_apple) and the quote.
It then sticks this binding in the Lib map, as mentioned in the evaluation section.
Now to push 'apple' you can do this:

    :> push_apple

Hooray, function definition! (You probably wondered how long it would take us to get here...)
Now about that messy stack...

If you've ever used Forth or Factor you might be familiar with the function `drop`.
Quark has no `drop` function by default, but we can make our own:

    :> [ x | ] :drop def

This is precisely how `drop` is defined in the standard library.
It takes one item, and puts back nothing.
Still, it will be a bit tedious to type `drop` several dozen times.
Thus we will utilize another new function: `match`

    :> [ [[ x | clear ]] match ] :clear def

We can now type `clear` to destroy the entire stack.
A bit dangerous, but I think you can handle it.
Why don't we walk through how `match` works.

`match` takes a quote of other quotes and tries them all until it finds one that matches.
Take this example:

    :> :cow [[:pig | 'oink'] [:cow | 'moo']] match

After it runs you should have a nice little cow noise on your stack.

We used `match` in the `clear` definition to recursively pop items off the stack until it was empty.
Pretty nifty...


"Luke, use the eval"
--------------------

Modern programming languages have a habit of implementing a function called `eval`.
They then demand you never use it, unless absolutely necessary.

Here at Quark Industries we think this behavior is pretty silly.
That's why we put in an `eval` function, and hope you use it too.
Check this out:

    :> ":8.0 :cow" eval
    :> .
    8.0 :cow :ok

Have a string containing Quark code?
Toss it at `eval` and the interpreter will evaluate it!
By now most programmers will feel rather uneasy about this whole thing, but stick with me.
`eval` is fairly safe, and as you can see above, it puts :ok on the stack if everything went well.
If you give it something like this:

    :> "[ pls no 39jd.a 3o" eval
    :> .
    :not-ok

`eval` will let you know if it can't handle something, instead of crashing the program.
Still, I suppose this might trip it up:

    :> "[rec] :rec def rec" eval

So don't do that.

One of the neat consequences of how simple Quark's syntax is, is the incredible ease with which it is serialized.
So we have `eval` to turn strings into code, but what about turning code into strings?
Meet, `show`

    :> [ x y | 4 :nimblefish ] show
    :> .
    "[ x y | 4.0 :nimblefish ]"

`eval` and `show` are basically inverse functions.
The only catch is that `eval` doesn't just parse, it also calls its argument.
We could (allthough we haven't) implement `call` like this:

    :> [ show eval ] :call def

I don't know if it's just me, but I think the ability to serialize and interpret arbitrary code is pretty darn cool.
The implications for metaprogramming are reason enough to justify this slightly dangerous feature.
As we'll see in a bit, these functions are used in the standard library for things like type conversion.

Do note, by the way, that state can be affected by `eval`, but is not captured by `show`.
By this I mean that you can define functions in `eval`, like this:

    :> "['Snail' print] :snail def" eval
    :> snail
    Snail

However `show` will never include defined functions, it merely turns the top item of the stack into a string.

Utilities
---------

In this section we'll take a quick look at many of the lesser functions in Quark.
To start us out, let's check out the stuff you can do with quotes:

    :> [ ] 4 << .
    [ 4.0 ]
    :> >> .
    [ ] 4.0
    :> <@ .
    [ 4.0 | ]
    :> @> .
    [ ] 4.0

Here we have two new sets of functions.
`<<` and `>>` push and pop items in quote bodies.
`<@` and `@>` do the same with quote patterns.

Moving on... (by the way, between these examples you might want to clear your stack):

    :> 16 4 4 * + .
    32.0
    :> 2 / .
    16.0
    :> 17 < .
    :false

Arithmetic! What wonders lie in store next?
You may have noticed that Quark doesn't have subtraction by default.
Quark does however have negative numbers, so subtraction is implemented like this:

    :> [ -1 * + ] :- def
    :> 7 4 - .
    3.0

Now how about string manipulation?

    :> "Tee" " Zeit!" weld .
    "Tee Zeit!"
    :> clear "abc" chars .
    ["a" "b" "c"]

These two functions are the only means of playing with strings in Quark.
The standard library however, implements many useful function with just `weld` and `chars`.

This concludes our function blitz, now on to interacting with the outside world...


IO, IO, it's off to the filesystem I go...
------------------------------------------

We've already met `print`, but as with most programming languages, Quark has more to offer in terms of input and output.
As a warning, I'm not entirely satisfied with the current lineup of io functions.
For example, there currently isn't a way to do networking, which rules out doing serverside code with Quark.
In the future I (or somebody more clever) may generalize or tweak these functions, to be more flexible.
Anyway, here are the two file functions:

    :> 'potato.txt' read .
    "Potatoes are usually brown."
    :> " They are also covered in eyes, like a Shoggoth!" weld 'potato_2.txt' write

There you have it, reading files and writing files, with `read` and `write`.
Not exactly rocket science.

There's also `cmd`:

    :> 'ls ~/shoggoth_pics' cmd .
    "cutie.png
    Elder_thing_4.jpg
    so_many_eyes.svg"

As you can see, it runs a command and puts the output as a string back on the stack.
This function is blocking, so your program will wait for the command to finish.

Last, but maybe not least, we have `exit`:

    :> [ [[ :cow | "No Cows!" print exit ] [ x | x ]] match ] :no_cows def
    :> :cow no_cows
    No Cows!
    :>

Now if you entered this into the repl, you are no doubt unimpressed, mainly because it didn't exit.
This is because `exit` only exits in interpreter mode.
In repl mode (because it gracefully handles errors) you'll have to enter `*q` (no whitespace) to exit.


Hindley–Milner!?
----------------

Despite being a proponent of strongly typed languages myself, Quark is as dynamic as they come.
Quark does have a type system though, as fluid and dangerous as it may be.
Why don't we poke at it?
Meet our final function (number 22), `type`:

    :> "Miskatonic University" type . clear
    :Str
    :> 5 type . clear
    :Num
    :> :dunwich type . clear
    :Sym
    :> summon_cthulhu type . clear
    :Atom

For these simple values, `type` will return a symbol which corresponds with the type of the value.
For quotes it's a little bit different:

    :> [ 1 2 3 ] type . clear
    [ :Quote :Empty :Num ]
    :> [ a | 2 3 ] type . clear
    [ :Quote :Atom :Num ]
    :> [ 5 :f 'shoggoth' ] type . clear
    [ :Quote :Empty :Any ]

When we call `type` on a quote it returns a quote whose first item is :Quote.
The second item is the type of the quote's pattern, and the third is the type of the quote's body.
But what's all this :Any and :Empty business?

When `type` checks the type of a quote's body or pattern, if the contents are homogeneous it returns that type.
So, [:a :b :d] would be of type :Sym
If however, multiple different types are present, like in example 3, it return :Any.
The type of an empty body or pattern is :Empty.

There you have it, a simple dynamic type system.
Because of `type` it's possible to build a typechecking system in Quark.
It is also possible to define functions which act differently on different types.


Something's missing...
----------------------

I can hear your puzzlement from here.
We just went through all 22 functions, and there was no import capabilities.
The truth is, in Quark import is just a composition of two functions we've already met: `load` and `eval`.
Your Quark distribution should have come with the standard library, why don't we load it up?
Hopefully you put it somewhere accessible.

    :> '~/Quark/stdlib.qrk' load eval

Now presuming you have :ok on your stack, you can play with all the goodies in there.
Check out the api documentation to figure out how all this stuff works.


Philosophy
----------

Here are some principles to follow when writing Quark code:

  1. Use point free style as much as possible.
  2. Functions should be as simple as possible.
  3. If a function goes across multiple lines, factor it.
  4. While it's possible to reassign atoms, try to avoid this.
  5. Function overloading is generally frowned upon.
  6. If a function contains large amounts of stack manipulation, use named pattern variables.
  7. All kinds of symbols are allowed in Quark atoms, use ? for boolean functions or ! for stateful ones.


Syntax
------

Here's a handy reference to what characters are allowed in Quark values:

Numbers can have 0..9 a dot '.' and then 0..9

Atoms can contain any of A..Z, a..z, and +-=&^%$#@!?/><,.;{}~ along with `_`, `*`, and backtick
(Irritatingly markdown is having a fit with some of the these characters so I had to separate the trouble makers)

Symbols are the same as Atoms, but start with ':'

Strings with "" can hold anything that isn't ", and '' strings can have anything but '

Quotes start with '[', may have a pattern ending with '|', and must conclude with ']'

Whitespace: tabs, spaces, and newlines, will all work.


Flaws
-----

By the end of this tutorial you've probably noticed several things you don't like about Quark.
Maybe a bump in the design here, or a discolored corner over there...
Well, I'm not one to hide the flaws in my creations...
You hold the wounds open, and I'll get the salt.

I am writing this small section after trying to bulk out the standard library with some new utility functions.
Using one's language is always the bane of any language designer.
It makes you furrow your brow and go: "Actually this is pretty rubbish..."
I don't actually think Quark is rubbish, mind you, but it has made me notice some rough edges.

The biggest issue I'm sure you'll face programming in Quark, is all the leaky abstractions.
Quark is a very mechanical language, in regards to it's evaluation.
It also has a large dependence on state, because of the stack.
Quark almost feels at times, like a big soft turing tarpit.
It has enough usability that we're not talking about Unlambda here, but it can still be a pain.

Another issue I've noticed is that Quark suffers from the same syntactic complexity found in Lisp.
Lisp proponents are eager to discount how painful it is to use a language with super regular syntax, I'm not so proud.
Keeping tabs on where you are in a match statement can be brutal, and is really only resolved by counting brackets.

Quark is also (at the moment) quite slow.
This is less of a flaw in the language, a more of a problem with the current implementation.
At this point in time the interpreter runs a script straight from the parsed abstract syntax tree.
Perhaps in the future a fast VM, or even a proper compiler, might solve this.
For now though, expect to wait two or three minutes when dealing with quotes containing thousands of items.


The End
-------

Quark gets it's name from an analogy.
In modern particle physics there are believed to be six elementary particles known as quarks.
These quarks (along with the leptons) compose together to create what we consider matter.
In Quark the key idea is that one can encode a vast amount of programming behavior in only 22 functions.
Through compositions of these 22 functions, all the features of the language can be implemented.
