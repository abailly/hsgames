# Build

```
$ idris2 --build bautzen1945.ipkg
```

# Run

Start server on some free port:

```
$ build/exec/bin/bautzen server --port 45678
```

Start client to connect to server in another terminal:

```
$ build/exec/bin/bautzen client --port 45678
>
```

**NOTE**: There's some deadlock somewhere in the code, so clients never get their answers back from server.

Send commands from client to server:

Request current state of the map:

```
> (:map?)
```

Request current positions of units:

```
> (:positions?)
```
