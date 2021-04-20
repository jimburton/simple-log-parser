# simple-log-parser
A demo of using AttoParsec to parse Apache access logs, based on the example at 
https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

A log file is a text file in which a server application records its
activity, such as the details of the clients that make requests from
the server, and any errors that occur whilst it runs (these two types
of information would normally be recorded in separate log files, the
*access* and *error* logs).

Log files may use different formats, and to make sense of their 
contents it is essential to know the format being used. The format we will
be using has one entry per line, where the lines look something like
this:

```
127.0.0.1 peter [09/02/2018:10:34:12] \"GET /sample-image.png HTTP/2\" 
55.0.123.1 - [09/02/2018:12:34:12] \"POST /index.php?key=value HTTP/1.1\"
```

The format can be described by the following expression:

```
%h %u %t \"%r\"
```

The elements of a log entry:

+ `%h` -- the IP address of the host
+ `%u` -- the userid of the person requesting the document as
  determined by HTTP authentication, or a hyphen (`-`) to indicate
  that the information is missing.
+ `%t`-- the time that the server finished processing the request.
+ `\"%r\"` -- the request line from the client enclosed in double quotes. 

This toy application can read log files in this format and parse the
contents into the Haskell datatype `LogEntry`. It provides a simple
example of using the parse combinator library `attoparsec`. 

Read the code in `src/Main.hs` and `src/SLP.hs`. Note the functions in
the `SLP` module that parse individual parts of a log entry, such as
the IP address or the user. As you can see from the type signatures,
each of these is a parser in its own right. Because `attoparsec` is a
parser *combinator* library we can combine these smaller parsers into 
a larger one that parses an entire entry. This is what is done in the
`logParser` parser function.

Build the code and try parsing the contents of the log file
`etc/tiny.log`:

```
$ git clone https://github.com/jimburton/simple-log-parser
$ cd simple-log-parser
$ cabal test
$ cabal run slp -- etc/tiny.log
```

The log file `etc/access.log` is written in a different format to `tiny.log`.
If you try parsing this one the resulting list of `LogEntry` values will
be empty.

```
$ cabal run slp -- etc/access.log
Up to date
[]

```

## Extension

The log format we are using is missing at least two important pieces
of information: the **status code** and the **size** of the requested
resource. The log file `etc/access.log` contains this information. It
is written using a format called the *Apache common format*. This is
the format:

```
%h %l %u %t \"%r\" %>s %b
```

The parts of the format that we haven't seen before:

+ `%l` -- the `RFC 1413` identity of the client determined by `identd`
  on the clients machine, or a hyphen (`-`) to indicate that the
  information is missing. This information is highly unreliable and
  should almost never be used except on tightly controlled internal
  networks.
+ `%>s` -- the status code that the server sends back to the client. 
+ `%b` -- the size of the object returned to the client, not including
  the response headers, or a hyphen (`-`) in the case where the response 
  contains no content.

Extend the parser to support the Apache common format. You will need
to extend the `LogEntry` data type to include the new fields then
write a parser for each type of information. 

Note that, like the user ID, the client identity is *either* a hyphen
or a string identifying the client. The status code is an integral
number. The size is either a hyphen or an integral number.


Once you have got this working you should be able to parse
`access.log` as above and run the tests successfully.
