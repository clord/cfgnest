# Introduction

I used to manage and maintain a very complicated pice of code that has a great deal of complex configuration options. Early on, this tool was based on nearly turing-complete text files, with nesting rules and complicated interactions. All of this needed to go, so myself and a small team set out to replace most of it. One of the components I factored out is a simplification of the configuration subsystem. The result I think is both novel and generally useful, so I'm open-sourcing it under 3-clause BSD. 

# CfgNest Model

Like most configuration tools, `cfgnest` is a nested key-value database with substitution. The main difference is that `cfgnest` does lookup differently — keys are searched from the leaves *out* to the root. This might sound initially like a bad idea, but it leads to several nice properties. Most configuration is boilerplate, and then details are provided by particular subsystems. The benefit of CfgNest is that if we arrange configuration with the most specific stuff at the leaves, then we can move all of the generic stuff up to the root where it can be shared.

For example, suppose you want to configure a pool of web-servers. Each will host a different application, but they all share certain things too. Let's set up a namespace for them:

    global/
    ├── domain = example.com
    ├── pid = 0
    └── webserver/
        ├── accesslog = /var/log/${hostname}/access.log
        ├── errorlog  = /var/log/${hostname}/error.log
        ├── killCmd   = kill ${pid}
        ├── fleet/
        │   ├── hostname = fleet.${domain}
        │   └── worker1
        │       └── pid = 1237
        │   └── worker2
        │       └── pid = 5784
        └── swift/
            ├── hostname = swift.${domain}
            └── worker1
                └── pid = 34783
            └── worker2
                └── pid = 38871

Each worker operates in it's context, for example `global/webserver/swift/worker1`. In this context, a `get 'killCmd'` operation  will produce "`kill 34783`", and `get 'errorlog'` will produce "`/var/log/swift.example.com/error.log`". Lookup always produces the most specific answer possible, but will go all the way to the root of the tree if necessary. This is effectively a tree of *nested* defaults. By taking advantage of this we reduced our configuration burden — from tens of thousands of lines of config to hundreds. 

# Usage

The API is currently fairly compact. For silly backwards compat reasons the code currently uses [MessagePack RPC][msgpack] for the wire representation on port 4040, in the future I might expose additional wire formats on different ports (I'm not 100% happy with MessagePack RPC for this application, although it buys compatibility with the software this tool replaced). The methods exposed are, in full:

## `get path key`

Fetch the key in the context of the path, with all variables expanded. produces a `nil` result if key does not exist. 

Runtime is $O(\text{len}_\text{path}+\text{log}(\text{numkeys}_\text{path}))$

## `has path key`

Determines if a key exists at that path (or any parent) and produces a boolean result. 

Runtime is $O(\text{len}_\text{path}+\text{log}(\text{numkeys}_\text{path}))$


## `del path key`

Deletes `key` at `path` iff it exists *at that path*. If it does not exist, or exists in a parent or child of the path, no action is taken. Result is empty. 

Runtime is $O(\text{len}_\text{path}+\text{log}(\text{numkeys}_\text{path}))$

## `set path key value`

Sets `key = value` at `path`. no result. 

Runtime is $O(\text{len}_\text{path}+\text{log}(\text{numkeys}_\text{path}))$

## `cfg expand path`

Fetches all available key/value pairs at `path`. If `expand` is set to true, all variables will be expanded, otherwise the original `${..}` syntax will be left intact.
 
## `children path`

Produce a list of child trees at a given path. 

## `deltree path key`

Delete a subtree named `key` at a given `path`.

## `tree path`

Fetch a representation of a subtree and all of it's children at `path`.


# Implementation

CfgNest is implemented with software transactional memory and in Haskell. It performs well enough for our application, but there are currently no provisions for sharding or other multi-node configuration. 

