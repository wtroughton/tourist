=============================
 Uniform Resource Identifier
=============================

The URI syntax is defined by :rfc:`3986`.

Each URI begins with a scheme, followed by authority, path, queries, and
fragment. e.g::
  
         foo://example.com:8042/over/there?name=ferret#nose
         \_/   \______________/\_________/ \_________/ \__/
          |           |            |            |        |
       scheme     authority       path        query   fragment

The URI syntax is organized hierarchically with components listed in order of
decreasing significance.

1. Scheme components are often named after protocols, e.g. ``http`` or ``ws`` for
   websocket.
2. The authority component is the namespace for hierarchical elements. 
3. The path component contains data that serves to identify a resource within the
   scope of the URI's scheme and naming authority.
4. Query components contains non-hierarchical data to identify a resource.
5. The fragment component are secondary resources.

Simplied ABNF
-------------

The full ABNF specification can be read from :rfc:`3986#appendix-A`. For a
simplified specification, we will implement the following::

   URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

   hier-part     = "//" authority path-abempty
                 / path-absolute
                 / path-rootless
                 / path-empty

   URI-reference = URI / relative-ref

   absolute-URI  = scheme ":" hier-part [ "?" query ]

   relative-ref  = relative-part [ "?" query ] [ "#" fragment ]

   relative-part = "//" authority path-abempty
                 / path-absolute
                 / path-noscheme
                 / path-empty

   scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

   authority     = host [ ":" port ]

   host          = IPv4address / reg-name
   
   port          = *DIGIT

   IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet

   dec-octet     = DIGIT                 ; 0-9
                 / %x31-39 DIGIT         ; 10-99
                 / "1" 2DIGIT            ; 100-199
                 / "2" %x30-34 DIGIT     ; 200-249
                 / "25" %x30-35          ; 250-255

   reg-name      = *( unreserved / pct-encoded / sub-delims )

   path          = path-abempty    ; begins with "/" or is empty
                 / path-absolute   ; begins with "/" but not "//"
                 / path-noscheme   ; begins with a non-colon segment
                 / path-rootless   ; begins with a segment
                 / path-empty      ; zero characters

   path-abempty  = *( "/" segment )
   path-absolute = "/" [ segment-nz *( "/" segment ) ]
   path-noscheme = segment-nz-nc *( "/" segment )
   path-rootless = segment-nz *( "/" segment )
   path-empty    = 0<pchar>

   segment       = *pchar
   segment-nz    = 1*pchar
   segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
                 ; non-zero-length segment without any colon ":"

   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

   query         = *( pchar / "/" / "?" )

   fragment      = *( pchar / "/" / "?" )

   unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
   reserved      = gen-delims / sub-delims
   gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
   sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                 / "*" / "+" / "," / ";" / "="

where

* ``ALPHA`` is the set of alpha characters which include both uppercase (A-Z) and
  lowercase (a-z) letters,
* ``DIGIT`` is the set of digit characters from 0-9.

Task
----

1. Write a function ``validURI :: Text -> Bool`` which takes a string and returns
   ``True`` if the string follows the simplified ABNF, ``False`` otherwise.

2. Given the record data type

   .. code-block:: haskell

      data URI = URI
        { scheme : undefined
	, authority : undefined
	, path : undefined
	, query : undefined
	, fragment : undefined
	}

   Fill in the undefined types with your own. These types can be nested or
   flat as you wish.

3. Write a parser function ``parseURI :: Text -> Maybe URI`` that takes in a
   string and returns the URI.

Examples
~~~~~~~~

The following example URIs illustrate several URI schemes and
variations in their common syntax components::

   ftp://ftp.is.co.za/rfc/rfc1808.txt

   http://www.ietf.org/rfc/rfc2396.txt

   ldap://[2001:db8::7]/c=GB?objectClass?one

   mailto:John.Doe@example.com

   news:comp.infosystems.www.servers.unix

   tel:+1-816-555-1212

   telnet://192.0.2.16:80/

   urn:oasis:names:specification:docbook:dtd:xml:4.1.2
