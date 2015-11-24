
A Brief (and/or long winded) Introduction to Quark
==================================================

Hello potential Quark user, before this guide can begin I'd like to run several health warnings by you.

  - Do you experience allergic reactions to brackets?
  - Does excessive stack manipulation make you break into a cold sweat?
  - Are you addicted to Lisp and/or Lisp like substances?

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

Assuming you managed to install Quark onto your IBM PC, PDP-11, or MIT Lisp Machine, let's fire up the REPL.
Quark has two modes of operation, interpreter and REPL.
To interpret a file, give `quark file-to-run.qrk` a try.
To start the REPL you can just type `quark` without any arguments.
Quark automatically loads the prelude (a minimal standard library) when it starts.
For this tutorial however, we only want the Quark core functions, so start the REPL like this:

    $ quark --only-core

If you managed to get this far you should see a prompt that looks like this:

    :>

Let's begin our examination of the language by looking at the data types Quark provides.
Why don't we start by pushing some numbers to the stack?

    :> 42 6.0 -0.976

Assuming the REPL didn't explode, it should have echoed the numbers we just entered.
If this is your first time using a stack based language, allow me to explain what we just did.
Every action in Quark takes place on the data stack, which is (as you probably guessed) a stack of data.
When the Quark interpreter receives literal values like the numbers we just entered, it pushes them to the stack.
After you've entered a bit of Quark code into the REPL, the interpreter will run it, then spit back the contents of the stack.

Now, back onto the subject of literals.
Numbers in Quark are all Doubles.
This simplifies things, and hopefully provides enough accuracy for most stuff.

Lets add some strings:

    :> "quarklang" 'single quotes work too...' 'chunky bacon'

Strings can be written with either single or double quotes.
I should point out here as well, that quark doesn't really care about line breaks.
Strings can span multiple lines if necessary, however the REPL doesn't really allow for this.
Here's the last, slightly more exotic, data type:

    :> :thing :Cows :the_color_blue

These are symbols, and if you're at all familiar with Ruby you should recognize them.
Symbols are literals used for names, keys, and other stuff.


Let's Get Functional!
---------------------

Functions are pretty simple too.
Quark makes a point of implementing by default only what is absolutely necessary.
For this reason Quark contains only a small set of built in functions (22 at the moment).
However, composing these core functions allows us to do all kind of neat things beyond Quark's basic functionality.

Quark belongs to a nifty set of functional languages, known as concatenative languages.
What this means is that Quark functions compose by default.
While in python one might write: `add(sub(5, 6), 9)` in Quark we can write: `5 6 sub 9 add`.
This kind of backwards function application (know as postfix notation) makes for some really clean code.
Being concatenative, also means that every individual part of a Quark program, is itself a program (even literals).

Enough with the rambling, on to functions!

Here is the first of our built-ins, `print`:

    :> 'Hello, world!' print

`print` takes a string off the data stack and prints it (pretty innovative).


You can Quote me on that...
---------------------------

There's just one more topic we have to address before we can dive into the deep end of the pool, Quotes.
Quotes are just lists of Quark literals or functions enclosed in brackets and delimited by whitespace, observe:

    :> [ 4 'fifty' print :orange ]

Items in a quote are frozen, meaning they won't execute until they're out of the quote.
Quotes serve two purposes:

  1. As a general purpose data structure, much like lists in LISP
  2. They allow us to compose functions, without applying them

In regards to point two, allow me to introduce the `call` function.

    :> [ "I'm in a quote..." print ] call

`call` takes a quote and unquotes it, by applying the quote's internals to the stack.
This means that `4` is the same as `[ 4 ] call`.


Pattern Matching, because if statements are for chumps...
---------------------------------------------------------

Quotes have one more property which is responsible for a lot of Quark's flavor, patterns.
Anyone who has used a functional programming language like Erlang, Haskell, or OCaml is probably familiar with pattern matching.
Quark fuses pattern matching with quotes, which let's us do all kinds of cool stuff.
Let's look at an example of this.

    :> [ 4 | :is_four ]

Patterns are little sub-lists written at the beginning of quotes, and separated from the quote body with a '|' character.
In this case our quote has a pattern of `4`.
When a pattern equipped quote is called, it completes 3 steps:

  1. Pop n items off the stack, where n is the length of the pattern (in the case of an empty pattern this is zero)
  2. Check if these popped items are equal to the items in the pattern (if the pattern item is a variable any value will match)
  3. If they don't match push the symbol :nil to the stack and put all the items back,
     otherwise if they do, Replace bound variables in the quote body and call the quote as explained above

Let's try this out:

  :> 6 [ 5 | 'This is a 5' print ] call

This will (presuming it isn't opposite day) *not* print 'This is a 5', instead as noted above it will put `:nil` on your stack.
Now let's do this:

    :> 'This will print!' [ x | x print ] call

Hopefully you can see what just happened:
  1. We popped the string off the stack
  2. Matched it with (and bound it to) the variable x
  3. Replaced the x in the body with 'This will print!'
  4. Called the quote

Let's do another!

    :> :cow 'pig' [ :cow other | 'There is a cow and a ' print other print ] call

There are just two more quote quirks to mention.

The first is that matching doesn't recursively check data.
That means, `[ [ a b ] | a ]`, won't do what you are probably expecting if you have a Haskell background.
For instance:

    :> [ :cow :pig ] [ [ :cow a ] | 'there's a cow and something else' ] call

The quote doesn't match because it only matches literal quote values with `:cow` and a variable named `a`.

The second quirk/feature is that pattern matching does identity checking.
That means this quote won't match:

    :> 4 5 [ x x | :same ] call

This feature allows us to define the equality function (`=`) with pattern matching.


Strikeless Matches
------------------

If `call` is the heart of Quark, then `match` is the brain.
`match` is like a supped up version of `call` that takes multiple quotes (in the form of one quote holding several).
It starts trying to match each quote from left to right, and when it finds one that does, it calls it.
Here's an example:

    :> 4 [[ 5 | :five ] [ 4 | :four ]] match

`match` is also stealthier than `call`.
When `match` can't find any matching quotes it leaves nothing on the stack, instead of `:nil`.

    :> 3 [[ 5 | :five ] [ 4 | :four ]] match

`match` comes in particularly handy in function definitions, which we'll address next.


I'm a bit def, you'll have to speak up
--------------------------------------

If you've been following these examples your data stack is probably filled with rubbish, let's do something about that...

To start, here's a new function: `def`
`def`, expects a stack of a quote and a symbol, like this:

    :> [ 'apple' print ] :print_apple def

`def`, will then create a binding between the function denoted by the symbol (in this case: `print_apple`) and the quote.
Now to print `'apple'` you can do this:

    :> print_apple

Hooray, function definition! (You probably wondered how long it would take us to get here...)
Now about that messy stack...

If you've ever used Forth or Factor you might be familiar with the function `drop`.
Quark has no `drop` function by default, but we can make our own with pattern matching:

    :> [ x | ] :drop def

This is precisely how `drop` is defined in the standard library.
It takes one item, and puts back nothing.
Still, it will be a bit tedious to type `drop` several dozen times.
Thus we will utilize `match` from the previous chapter:

    :> [ [[ x | clear ]] match ] :clear def

We use `match` in the `clear` definition to recursively pop items off the stack until it's empty.
Pretty nifty...
We can now type `clear` to destroy the entire stack.
A bit dangerous, but I think you can handle it.


"Luke, use the eval"
--------------------

Modern programming languages have a habit of implementing a function called `eval`.
They then demand you never use it, unless absolutely necessary.

Here at Quark Industries we think this behavior is pretty silly.
That's why we implemented an `eval` function, and hope you use it too.
Check this out:

    :> "8.0 :cow" eval

Have a string containing Quark code?
Toss it at `eval` and the interpreter will evaluate it!
By now most sane programmers will feel rather uneasy about this whole thing, and I admit that `eval` can be dangerous.
Remember though, that Quark focuses on flexibility and expressiveness over safety.
`eval` is a powerful tool, and as long as you don't do anything too crazy, everything should be fine...
`eval` is even somewhat safe.
As you can see above, it puts `:ok` on the stack if everything went well.
However, if you give it something like this:

    :> "[ pls no 39jd.a 3o" eval

`eval` will let you know when it can't parse something by pushing `:not-ok`, instead of crashing the program.
Still, I suppose this might trip it up:

    :> "[rec] :rec def rec" eval

So don't do that.

One of the neat consequences of how simple Quark's syntax is, is the incredible ease with which it is serialized.
So we have `eval` to turn strings into code, but what about turning code into strings?
Meet, `show`

    :> [ x y | 4 :nimblefish ] show print

`eval` and `show` are basically inverse functions.
The only slight difference is that `eval` doesn't just parse, but also calls its argument.

I don't know if it's just me, but I think the ability to serialize and interpret arbitrary code is pretty darn cool.
The implications for metaprogramming are reason enough to justify this slightly dangerous feature.
For example, these functions are used very easily for things like type conversion.

Do note, by the way, that state can be affected by `eval`, but is not captured by `show`.
By this I mean that you can define functions in `eval`, like this:

    :> "['Snail' print] :snail def" eval

However `show` will never include defined functions, it merely turns the top item of the stack into a string.


Utilities
---------

In this section we'll take a quick look at many of the lesser functions in Quark.
To start us out, let's check out the stuff you can do with quotes:

    :> [ ] 4 <<
    :> >>

The `<<` and `>>` are inverse functions that push and pop items into quote bodies.

For dealing with quote patterns, we have a second pair of inverse functions, `@+` and `@-`.
`@-` splits a quote with a pattern into two quotes with bodies containing the pattern and body of the original quote.
`@+` does the opposite, by joining two quote's bodies into a single quote with the first as a body and the second as a head.

    :> [ 1 2 3 | :a :b :c ] @-
    :> @+

Moving on... (by the way, between these examples you might want to clear your stack):

    :> 16 4 4 * +
    :> 2 /
    :> 17 <

Arithmetic! What wonders lie in store next?
You may have noticed that Quark doesn't have subtraction by default.
Quark does however have negative numbers, so subtraction is implemented like this:

    :> [ -1 * + ] :- def
    :> 7 4 -

Now how about string manipulation?
The function `weld` will concatenate two strings.
Also, `chars` will break a string into a quote containing each of the string's characters.

    :> "Tee" " Zeit!" weld
    :> "abc" chars

This concludes our function blitz, now on to interacting with the outside world...


IO, IO, it's off to the filesystem I go...
------------------------------------------

We've already met `print`, but as with most programming languages, Quark has more to offer in terms of input and output.
As a warning, I'm not entirely satisfied with the current lineup of io functions.
For example, there currently isn't a way to do networking, which rules out doing serverside code with Quark.
In the future I (or somebody more clever) may generalize or tweak these functions, to be more flexible.
Anyway, here are the two file functions:

    :> 'potato.txt' read
    :> "Potatoes are covered in eyes, like a Shoggoth!" 'potato_2.txt' write

There you have it, reading files and writing files, with `read` and `write`.
Not exactly rocket science.

There's also `cmd`:

    :> 'ls ~/shoggoth_pics' cmd

As you can see, it runs a command and puts the output as a string back on the stack.
This function is blocking, so your program will wait for the command to finish (and if it never does, you're out of luck).

You may have noticed that all of these function add a `:ok` to your stack, just like `eval`.
This allows us to check if the operation succeeded.
If it didn't these functions will return a `:not-ok`.

The REPL prints out the stack by default, but there may be a time when you want to do this manually.
For that you can use `.` which prints out the entire stack.

    :> 324 :cabbage [ 'meow' ] .

Last, but maybe not least, we have `exit`:

    :> [ [[ :cow | "No Cows!" print exit ]] match ] :no_cows def
    :> :cow no_cows

Now if you entered this into the REPL, you are no doubt unimpressed, mainly because it didn't exit.
This is because `exit` only exits in interpreter mode.
In REPL mode (because it gracefully handles errors) you'll have to enter `*q` (no whitespace) to exit.


"Special" Functions
-------------------

The Quark interpreter also implements two magic REPL only functions.

As we saw above, we can quit the REPL by entering `*q`.

Also available is the REPL function `*f`.
Typed alone `*f` will print out all user defined functions and their accompanying binding.
`*f` can also target a specific function to print out.

    :> \*f
    :> \*f drop


Hindley–Milner!?
----------------

Despite being a proponent of strongly typed languages myself, Quark is as dynamic as they come.
Quark does have a type system though, as fluid and dangerous as it may be.
At runtime Quark will check to make sure core functions are being applied to the right arguments.
Besides, that the type system tries to stay out of the way as much as it can.

Here's our final function, `type`.
`type` returns a symbol which corresponds with the type of the value:

    :> "Miskatonic University" type
    :> 5 type
    :> :dunwich type
    :> [ 1 8 9 0 ] type

Because of `type` it's possible to build a fancier typechecking system in Quark.
In the future hopefully the prelude will have one included, but for now you'll have to write your own.


Modules
-------
I have to confess that the title of this chapter is a bit of a lie.
Unlike proper languages, Quark doesn't have a fancy module system.
It can be handy to separate programs into multiple files though...

Requiring files in Quark can be expressed as a composition of two functions we've already met: `load` and `eval`.
Your Quark distribution should have come with the standard library, so why don't we load it up?
Hopefully you put it somewhere accessible.

    :> '~/quark/prelude.qrk' load eval

Now presuming you have `:ok` on your stack, you can play with all the goodies in there.
Check out the API documentation to figure out how all this stuff works.
For convenience's sake Quark will use the prelude by default, so you won't have to add `load eval` to all your scripts.


Tips
----

Here are some things to remember when writing Quark code:

  1. Factor, factor, factor, and then factor again.
     Because Quark is a concatenative language factoring is very easy.
     Anytime you find yourself reusing some behavior, make it into a new function.

  2. Non-stateful is better than stateful.
     While it's possible to reassign atoms, try to structure your code so that this isn't necessary.

  3. Using quote patterns is better than excessive use of stack manipulation functions.
     This is a major problem for Forth and Factor code, which is often littered with `swap`s and `rot`s.
     If you find yourself using lots of these functions, re-factor using pattern variables instead.

  4. Be creative with function names.
     Quark lets you use all kinds of symbols in function names, use that to your advantage.
     End boolean returning functions with `?` or use `'` to indicate a new version of a value.


Syntax
------

`number ::= /-?[0-9](.[0-9])?/`

`atom ::= /[^0-9\[\]|:"'\s\n\t]+/`

`symbol ::= <':'> <atom>`

`string ::= /'[^']*'/ | /"[^"]*"/`

`quote ::= <'['> ?(<qexpr> <'|'>) ?(<qexpr>) <']'>

'qexpr ::= <number> | <atom> | <symbol> | <string> | <quote> | <qexpr> <qexpr>`


Flaws
-----

By the end of this tutorial you've probably noticed several things you don't like about Quark.
Maybe a bump in the design here, or a discolored corner over there...
Well, I'm not one to hide the flaws in my creations...
You hold the wounds open, and I'll get the salt.

I am writing this small section after trying to bulk out the standard library with some new utility functions.
Using one's language is always the bane of any language designer.
It makes you furrow your brow and go: "Actually this is pretty rubbish..."
In actuality don't think Quark is rubbish, but it has made me notice some rough edges.

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
In Quark the key idea is that one can encode a vast amount of programming behavior with a small set of core functions.
Through compositions of these core functions, all the features of the language can be implemented.
