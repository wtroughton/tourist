= Aeson

Aeson is for converting between Haskell types and JSON objects. There are two
main classes used in Aeson -- `FromJSON` and `ToJSON`. 

There are also two functions for reading JSON, `decode` and converting to JSON,
`encode`. 

== Reading JSON values with FromJSON

For converting from JSON to a Haskell type, consider Aeson as a specialized
parser for JSON strings. You'll find that many Haskell libraries make use of the
Parser monad, and Aeson is no exception. Reading JSON strings and parsing into
Haskell data type fits the bill of parsers.

=== Aeson Datatypes

Aeson represents JSON using its defined type, the `Value` type.

```
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
```

So, if you want to construct JSON directly, you can do it by constructing a
`Value` and then encode it.

Similarly, given a JSON string, we can write a function that accepts a `Value`
and returns our object type like so:

```
parseTuple :: Value -> Parser (String, Bool)
```

```
data Currency = USD | BTC

instance FromJSON Currency where
    parseJSON = undefined
```

```
encode USD
"\"USD\""
```

```
data Money = Money {
    amount :: Number,
    currency :: Currency
}

instance FromJSON Money where
    parseJSON = undefined
```

