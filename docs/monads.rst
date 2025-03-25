===============================
 Functors, Applicative, Monads
===============================

Functors
--------

A very basic concept of Haskell is the application of a function to a value.

.. code-block:: haskell
   
   (*5) 2
   
Sometimes, we have values contained in a context. Examples: ``Just 2``, ``[2]``;
We cannot directly apply the function ``(*5)`` to ``Just 2`` or ``[2]``, since
we first have to obtain the value 2 from its context.

Consider the context as a box that contains the value. For lists, we have a
function ``map`` to apply a function to all list elements. A generalization of
``map`` is the function ``fmap :: (a -> b) -> f a -> f b``.

.. code-block:: haskell
		
   map (*5) [2]
   fmap (*5) [2]
   fmap (*5) (Just 2)
   fmap (*5) Nothing

Types that implement ``fmap`` are called functors. A functor is the type class:

.. code-block:: haskell

   class Functor f where
       fmap :: (a -> b) -> f a -> f b

The fmap function accepts a function who takes an argument a and transforms it
to some value b, and a functor f that contains a and returns the same functor f
containing b. All functors must implement fmap. Functors are common enough in
Haskell that there is the special operator ``<$>`` for fmap.

.. code-block:: haskell

   fmap (*5) [2]
   (*5) <$> [2]

   fmap (*5) (Just 2)
   (*5) <$> (Just 2)

Functions are also functors. We can use this fact to compose functions.

.. code-block:: haskell

   f = fmap (*5) (+5)
   g = (*5) <$> (+5)
   h = (*5) . (+5)

Functors are used for applying functions to values inside container
types. Functors have two laws: identity law and composition law.

.. code-block:: haskell

   fmap id = id

   (fmap f) . (fmap g) = fmap (f . g)

Applicative Functors
--------------------

The Applicative type class extends the Functor type class. Applicative supports
functions wrapped in a context.

.. code-block:: haskell

   class Functor f => Applicative f where
      pure  :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

The function ``pure`` takes a value and puts it into a context. The operator
``(<*>)`` applies a function in a context to a value ``a`` in a functor and
returns a value ``b`` in the same functor. The apply operator allows us to chain
operations by wrapping a function. 

.. note::

   The ``pure`` function can be thought of as lifting a value into a context. It
   is a wrapper around a normal object.

.. code-block:: haskell

   Just (*5) <*> Just 2

   [(*5)] <*> [2]

   [(*1), (*2), (*3)] <*> [1..3]

   ((*) <$> (Just 5)) <*> (Just 2)

Here's the instance for ``Maybe``::

  instance Applicative Maybe where
    pure = Just
    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing
    (<*>) (Just f) (Just x) = Just (f x)

Applicatives differ from functors in that they let us combine wrapped
data. Consider trying to multiply two Maybe values::

  >> (Just 4) * (Just 2)
  >> Nothing * (Just 2)

We could wrap our multiply function using fmap like so: ``f = (*) <$> (Just 4)``
This gives us a partial function wrapped in a Maybe. We would still need to
write code to write a function to apply it to ``Just 2``.

.. code-block:: haskell

   funcMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
   funcMaybe Nothing _ = Nothing
   funcMaybe (Just f) value = f <$> value

We can write this neatly with Applicative.::

  >> pure (*) <*> Just 4 <*> Just 2
  >> (*) <$> Nothing <*> Just 2

Monads
------

The Monad type class extends the Applicative type class. A monad is a
computational context that allows you to chain together operations that have
some kind of shared state or similar effect.

.. code-block:: haskell

   class (Applicative m) => Monad m where
     return :: a -> m a
     (>>=) :: m a -> (a -> m b) -> m b

The type class defines a function ``return`` that puts a value into a monad and
a function ``(>>=)`` called *bind* that takes a value in a monad and returns a
value in a possibly different monad and returns the monadic value.

.. code:: haskell

   half :: Int -> Maybe Int
   half x = if even x
            then Just (x `div` 2)
	    else Nothing

We cannot compose ``half`` with itself since ``half`` takes an Int and returns a
Maybe Int. The bind operator can help.

.. code:: haskell

   Just 8 >>= half
   Just 4 >>= half
   Just 2 >>= half
   Just 1 >>= half

   return 8 >>= half >>= half >>= half

Monads play an important role in Haskell since they can be used to encapsulate
side effects. For example, the IO Monad takes care of input and output
operations. The ``getLine :: IO String`` function takes no arguments and returns
an IO action to read a string from an input. The ``putStrLn :: String -> IO
String`` takes a string and returns an IO action to print it. These functions
can be chained together:

.. code:: haskell

   getLine >>= putStrLn

In case the result of a chained function is not needed, we can use the ``then (>>)``
operator.

.. code:: haskell

   putStr "Hello" >> putStr " " >> putStr "world"

Haskell has special notation for monads, the ``do`` notation. 

`Reference <https://cnds.constructor.university/courses/ics-2019/hs-funappmon.pdf>`_

..
   Local Variables:
   jinx-local-words: "Applicatives"
   End:
