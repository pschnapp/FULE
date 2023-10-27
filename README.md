# FULE

The Functional UI Layout Engine (FULE - pronounced "fuel") is an experimental Haskell library for positioning UI elements on-screen. The layout will react dynamically to changes in window-size or manipulations by the user, but it is up to you to wire these events from the GUI framework to the FULE layout and to draw the components themselves on the screen.

Check out the [usage documentation](doc/Usage.md) and [examples](examples/) for more information, or if curious you can dive into [the theory behind it](doc/Theory.md).

The [osabe project](https://bitbucket.org/osabe-app/code/) can be referenced as an example use of the library -- it is for this appliction that FULE was written in the first place.

If you have any questions or issues feel free to contact me. I'd also be interested to hear about any uses of this library.

## Features
 - Pre-defined containers (more to come!)
 - Easy centering of content
 - Dynamic, responsive layout
 - Flat data-structures
 - Monad-friendly API

## Stability

Basic usage of the latest commit should be stable.

The only thing that's still a WIP is _constraints_, and that should only cause issues when using the `Divided` container in `dynamic` mode.

## Roadmap

### Planning
 - A `Quartered` container
 - A `Table` container

### Wishlist
 - Removable/replaceable components -- updatable `Layout`
 - An `Oversized` container (for scrolling)
 - Have `Positioned` take effect within an `Unreckoned` container
