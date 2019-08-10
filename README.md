# About

An HTTP server for show information about a [Unison](http://unisonweb.org/) codebase, along with an Elm client for viewing it.

Inspired by [haskell-code-explorer](https://github.com/alexwl/haskell-code-explorer).

# Status

Heavily WIP-- basically the only thing working is the server generating a function call graph.

# Use

Start the server:
```sh
$ cd server

$ stack install
<lots of output>

$ unison-code-explorer
<lots of logs about skipping references>
listening on port 3000
```

# API

### http localhost:3000/function-call-graph

```json
[
  [
    "0vjv37iqunnu4tvi3jiu3mees01le0278qs0sgmbgisoil83991351tgcqei5ke9flelqocsbh7n18t0n2s1ijorde2l3ol22327mv8",
    [
      "fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g",
      "rultimjsuqimjid9ktklj6n26ejvsbn21d1d161vk54hf7a7nmic96c1aefb4jhoko520e6fbtetje3fe54i4mhdjrh3p7movr3uk70"
    ]
  ]
]
```

### http localhost:3000/names

```json
[
  [
    "0vjv37iqunnu4tvi3jiu3mees01le0278qs0sgmbgisoil83991351tgcqei5ke9flelqocsbh7n18t0n2s1ijorde2l3ol22327mv8",
    "io.renameFile"
  ],
  [
    "11l799s1o1r5bs47rrbrjngqkun1r5o4plffdlblvedbqbfe966ubllbfv53kn5cpgsinjogd873iuiaaoeun5liqt9vp9t6jkdlhm0",
    "io.createDirectory"
  ]
]
```
