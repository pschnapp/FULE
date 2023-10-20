# FULE

The Functional UI Layout Engine (FULE - pronounced "fuel") is an experimental Haskell library for positioning UI elements on-screen. The layout will react dynamically to changes in window-size or manipulations by the user, but it is up to you to wire these events from the GUI framework to the FULE layout and to draw the components themselves on the screen.

Check out the [usage documentation](doc/Usage.md) and [examples](examples/) for more information, or if curious you can dive into [the theory behind it](doc/Theory.md).

The [osabe project](https://bitbucket.org/osabe-app/code/) can be referenced as a sample use of the library -- it is for this appliction that FULE was written in the first place.

If you have any questions or issues feel free to contact me. I'd also be interested to hear about any uses of this library.

## Stability

Basic usage should be stable, especially when using containers that rely on plastic relationships.

Deeply nesting containers that rely on elastic relationships (`Grid`, anything with centering) will lead to improper behavior during size changes, and this is an area still under development.

It is recommended to keep your nesting of elastic containers to *two levels* if there will be resizing -- the initial layout works fine for deeper levels of nesting though if things will be static.

## Roadmap

### Planning
 - A `Quartered` container
 - A `Table` container

### Wishlist
 - Removable/replaceable components -- updatable `Layout`
 - An `Oversized` container (for scrolling)
 - Have `Positioned` take effect within an `Unreckoned` container
