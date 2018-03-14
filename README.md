# simple-log-parser
A demo of using AttoParsec to parse Apache access logs, based on the example at 
https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

Logs are expected to be in the following format:

    %h %u %t \"%r\"

Yielding log entries that look like this:

    127.0.0.1 peter [09/02/2018:10:34:12] \"GET /sample-image.png HTTP/2\"" 
	55.0.123.1 - [09/02/2018:12:34:12] \"POST /index.php?key=value HTTP/1.1\"" 

The elements of a log entry:

+ `%h` -- the IP address of the host
+ `%u` -- the userid of the person requesting the document as
  determined by HTTP authentication, or a hyphen (`-`) to indicate
  that the information is missing.
+ `%t`-- the time that the server finished processing the request.
+ `\"%r\"` -- the request line from the client enclosed in double quotes. 

Usual rigmarole:

    $ git clone https://github.com/jimburton/simple-log-parser
	$ cd simple-log-parser
	$ cabal sandbox init
	$ cabal configure
	$ cabal install
	$ cabal test
	
## Extension

The log format we are using is missing at least two important pieces
of information: the **status code** and the **size** of the requested
resource. The Apache common format includes this information:

    %h %l %u %t \"%r\" %>s %b

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

Extend the parser to support the Apache common format. You will need to 
extend the `LogEntry` data type to include the new fields then write a
parser for each type of information. 

Note that, like the user ID, the client identity is *either* a hyphen or a string
identifying the client. The status code is an integral number. The size is
either a hyphen or an integral number.
