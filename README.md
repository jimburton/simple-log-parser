# simple-log-parser

A demo of using AttoParsec to parse access logs from a web server. Logs are
expected to be in the following format:

    %h %u %t \"%r\"

yielding log entries that look like this:

    127.0.0.1 peter [09/Feb/2018:10:34:12 +0000] "GET /sample-image.png HTTP/2" 
	55.0.123.1 - [09/Jun/2018:12:34:12 +0000] "POST /index.php?key=value HTTP/1.1" 

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
	$ cabal configure --enable-tests
	$ cabal install
	$ cabal test
	
As well as running the tests, you can run the executable itself using `cabal run`. The main method expects one argument 
which is the path to a log file. `cabal` sends everything after a double hyphen as an argument to the executable:

    $ cabal run -- etc/tiny.log
	Running slp...
    Right [LogEntry {entryIP = IP 127 0 0 1, entryUser = User "peter", entryTime = 2018-02-09 10:34:12, entryReq = "GET /sample-image.png HTTP/2"}]
	
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
parser for each type of information. Then modify the `logEntryParser`
function to construct the new fields.

Note that, like the user ID, the client identity is *either* a hyphen or a string
identifying the client. The status code is an integral number. The size is
either a hyphen or an integral number.

There is a `HUnit` test in `tests/Main.hs` that tries to parse the log file
`etc/access.log`, which is in common format. The test should begin to pass once 
your new parser is working. 
