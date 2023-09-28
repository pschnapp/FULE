# FULE

The Functional UI Layout Engine (FULE - pronounced "fuel") is an experimental Haskell library for positioning UI elements on-screen. The layout will react dynamically to changes in window-size or manipulations by the user, but it is up to you to wire these events from the GUI framework to the FULE layout and to draw the components themselves on the screen.

Check out the [usage documentation](doc/Usage.md) and [examples](examples/) for more information, or if curious you can dive into [the theory behind it](doc/Theory.md).

The [osabe project](https://bitbucket.org/osabe-app/code/) can be referenced as an example use of the library -- it is for this appliction that FULE was written in the first place.

If you have any questions or issues feel free to contact me. I'd also be interested to hear about any uses of this library.

## Feature Wishlist
 - Removable/replaceable components
 - A `Quartered` container
 - An `Oversized` container
 - A `Table` container
 - Have `Positioned` take effect within an `Unreckoned` container
