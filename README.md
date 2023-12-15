# FULE

The Functional UI Layout Engine (FULE - pronounced "fuel") is an experimental Haskell library for positioning UI elements on-screen. The layout will react dynamically to changes in window-size or manipulations by the user, but it is up to you to wire these events from the GUI framework to the FULE layout and to draw the components themselves on the screen.

Check out the [usage documentation](https://github.com/pschnapp/FULE/blob/main/doc/Usage.md) and [examples](https://github.com/pschnapp/FULE/blob/main/examples) for more information, or if curious you can dive into [the theory behind it](https://github.com/pschnapp/FULE/blob/main/doc/Theory.md).

The [osabe project](https://bitbucket.org/osabe-app/code/) can be referenced as an example use of the library -- it is for this appliction that FULE was written in the first place.

If you have any questions or issues feel free to contact me. I'd also be interested to hear about any uses of this library.

## Features
 - Pre-defined containers (more to come!)
 - Easy centering of content
 - Dynamic, responsive layout
 - Flat data-structures
 - Monad-friendly API

## Roadmap

### Planning
 - A `Quartered` container
 - A `Table` container

### Wishlist
 - Removable/replaceable components -- updatable `Layout`
 - An `Oversized` container (for scrolling)
 - Have `Positioned` take effect within an `Unreckoned` container
