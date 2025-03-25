====================
 Parser Combinators
====================

.. rubric:: Prerequisites

* Higher order functions

  * maps, foldr
  * reduce

To build intuition with functors, applicatives and monads, we'll work through
some real world examples using parsers.

We will demonstrate how to build higher order parsers from primitive parsers.

Consider we wish to parse command line arguments using the `OptParse
<https://github.com/pcapriotti/optparse-applicative>`_ library.

.. highlight:: haskell

.. code-block::
   :linenos:

   import Options.Applicative

   data Sample = Sample
     { hello :: String
     , quiet :: Bool
     , enthusiasm :: Int }

   sample :: Parser Sample
   sample = Sample
     <$> strOption
         ( long "hello"
	<> metavar "TARGET"
	<> help "Target for the greeting" )
     <*> switch
         ( long "quiet"
	<> help "Whether to be quiet" )
     <*> option auto
         ( long "enthusiasm"
	<> help "How enthusiastically to greet"
	<> showDefault
	<> value 1
	<> metavar "INT" )

At first this looks rather cryptic. But if we look at the contents and its
structure, we can work out some details:

The top level headers, ``strOption``, ``switch``, ``option auto``, each have a
``long`` and ``help`` function. The ``option auto`` node has a default value.
  
The ``sample`` function returns the ``Parser`` monad containing type
``Sample``. We can infer that strOption, switch and option auto return a string,
bool, and string respectively.

Recall:

* ``<$>`` is the ``fmap`` operation.

  * ``(*) <$> Just 5`` returns ``Just (*5)``
    
* ``<*>`` is the ``apply`` operation.

  * ``Just (*) <*> Just (5)`` returns ``Just (*5)``

The ``Sample`` function is a constructor that is being applied (``<$>``) to some
blob ``_`` such that it returns some ``Parser Sample``. In that case, we can
infer that ``strOption``, ``switch`` and ``option auto`` are of the monad
``Parser ???``. And thanks to applicative, we can apply operations between
Parsers.

.. code-block::

   Sample <$> Parser String <*> Parser Bool <*> Parser String

.. admonition:: Question
		
   What do you think the type signatures are for the following functions:

   1. strOption
   2. switch
   3. option
   4. auto

   Verify your answers with the `source code.
   <https://hackage.haskell.org/package/optparse-applicative-0.18.1.0/docs/src/Options.Applicative.Builder.html>`_
   Is it what you expected?

Combinators
-----------

A ``Parser a`` is an option parser returning a value of type ``a``.

.. code-block::

   strOption :: IsString s => Mod OptionFields s -> Parser s

Takes an optional String argument.

.. seealso::
      
   `Megaparsec <https://markkarpov.com/tutorial/megaparsec.html>`_
      Tutorial for general purpose parsers

   `Functors <https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html>`_
      Article about functors, applicatives and monads

.. admonition:: Example

   This example is collapsible. [#f1]_


.. [#f1] Not yet released as of Feb. 15, 2025
