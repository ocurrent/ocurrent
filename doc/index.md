Welcome to the OCurrent documentation!

### Prerequisites

Once you have checked out the ocurrent code, please ensure you have up to date submodules, and
the required opam dependancies installed:

```
$ git submodule update --init --recursive
$ opam install . --deps-only
```

### Introduction

* [Example pipelines](example_pipelines.md)
* [Skeleton project](https://github.com/ocurrent/ocurrent-skeleton) - a template for new projects
* [Internals](internals.md) - how it all works
* [API docs](https://ocurrent.github.io/ocurrent/index.html)

### OCurrent plugins

* [Current_docker](https://ocurrent.github.io/ocurrent/current_docker/index.html)
* [Current_fs](https://ocurrent.github.io/ocurrent/current/Current_fs/index.html)
* [Current_git](https://ocurrent.github.io/ocurrent/current_git/index.html)
* [Current_github](https://ocurrent.github.io/ocurrent/current_github/index.html)
* [Current_gitlab](https://ocurrent.github.io/ocurrent/current_gitlab/index.html)
* [Current_slack](https://ocurrent.github.io/ocurrent/current_slack/index.html)
* [Current_rpc](https://ocurrent.github.io/ocurrent/current_rpc/index.html) - control OCurrent remotely via Cap'n Proto
* [Current_ssh](https://ocurrent.github.io/ocurrent/current_ssh/index.html)

Or you can [write your own plugin](writing_plugins.md).

### Design and implementation details

* [The on-disk cache format](disk_cache.md)
