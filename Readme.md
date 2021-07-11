[![Build status](https://img.shields.io/travis/jappeace/haskell-template-project)](https://travis-ci.org/jappeace/haskell-template-project/builds/)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-template-project/Test)](https://github.com/jappeace/haskell-template-project/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

> There are too many monads.

Intersperse library allows some sideffect to be called for every bind
call.
This allows you to measure progress in some background job for example.

# Usage

Consider some program you have:
```
someProgram :: MonadIO m => m Char
someProgram = do
      liftIO $ putStrLn "hello " -- 0
      liftIO $ putStrLn "world"  -- 1
      pure 'x' -- 2

```
Let's say you want to know how many bind calls there are,
first we need to define the AppMonad that can run in:
```
-- some appstack, can be arbeterarly complex. We need a newtype to avoid orphans.
newtype ProgramCounterTestM m a = MkProgramCounterTest { unIORef :: ReaderT (IORef Int) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef Int) )
```

Add the callback for counting
```haskell
-- the instance decides what to interspserse, we can use newtypes to make multiple for a single monad stack
instance BeforeBindCall (ProgramCounterTestM IO) where
  before = do
    ref <- ask
    liftIO $ modifyIORef ref (+1)

```

The unit test then shows how to run the program well:

```haskell
  it "can program count, (assign a number to each bind call)" $ do
    ref <- newIORef 0
    void $ flip runReaderT ref $ unIORef
         $ runIntersperse someProgram -- the runIntersperse call will tell you what instance to provide (as a type error)
    res <- readIORef ref
    res `shouldBe` 2

```

# Alternatives

+ https://hackage.haskell.org/package/interspersed
  Uses a runtime state monad to intersperse effects.
  My implementation has no additional runtime costs.
  
+ https://hackage.haskell.org/package/logict-0.7.1.0/docs/Control-Monad-Logic-Class.html#v:interleave
  

# FAQ
flamboyantly alternating queries.

## Is this lawful?
Yes.
I had to copy some laws to make the right number of before calls appear.
For example, fmap will now also result in a before call because of a monad law.
Applying `(<*>)` results in two before calls.
 
But, does lawfulness really matter if it's useful? 
Most of mtl is a big hack.

## Can I use this to create a debugger
Not for arbitrary function calls:
Yes, function arrow `(->)` is a monad, but
the function application mechanism isn't overidable as far as I know,
and it needs to be mtl style, all current code is written with the concrete
base monad `(->)`, rather then `(id :: IsFunction fun => fun a a)`
and it also needs to be able to get transformers attached.

It will work for stepping trough side effects.
It could work if you modify the runtime.

## I have asimlar idea but want to put a bracket around every bind call
Make a PR, I'll merge it. It's related enough. I just don't need that
right now so I didn't build it.


## Can you explain this using drugboats.

!(https://www.youtube.com/watch?v=O2rGTXHvPCQ)[]

of course.

Imagine a bind call being some drug boat coming in the harbor,

While the drugboat is looking for the right harbor, we quickly sneak in
another drugboat to do some defined in `BeforeBindCall`.
For example we could tell the sneaking in drugboat to always add
a packet of drugs in the men's bathroom of the local bar.

Then he leaves and the other drugboat does it's normal operations.
Such as buying and selling drugs.

Now the first drugboat may influence the second drugboat, but they never meat.
The second drugboat doing his normal buying and selling business may for example 
acidently find the drugs in the mens' bathroom and get a massive
profit.
However, because they never meet none of the drugboats have to
be modified to deal with meetings.
