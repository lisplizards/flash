# flash

## Usage

### Clack

Ensure that the app is wrapped in session middleware.

```lisp
(funcall lack/middleware/session:*lack-middleware-session*
         (funcall lack/middleware/flash:*lack-middleware-flash*
                  *app*))

```

Lack Builder:

```lisp
(lack:builder
  :session
  :flash
  *app*)
```

The middleware adds `:FLASH` to the Clack environment and dynamically binds
`FOO.LISP.FLASH:*FLASH*`. The `FOO.LISP.FLASH` package defines functions
used to operate on this flash-state object.

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

So as to enable integrations with non-Clack compatible web servers,
this git repository contains two ASDF systems:

* `foo.lisp.lack-middleware-flash`
* `foo.lisp.flash`

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-middleware-flash)

(asdf:test-system :foo.lisp.flash)
```

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
