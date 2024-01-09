# Introduction

FULE is a layout engine for calculating the positions of visual elements on screen and adjusting these positions in response to input. While it does not draw the the elements or facilitate interactions with them -- these are left up to you -- it does try to give you a leg up in your effort to do so!

Creating a layout with FULE can be done at a high level utilizing its predefined containers, at a low level with the `LayoutDesign` type using guides, or at a mix of both levels. There are some simple examples in the [examples](../examples/) directory if you'd like to view some working code.

This document goes over FULE's essential concepts and discusses how to integrate FULE into your project.

 - [Essential Concepts](#essential-concepts)
 - [Imports](#imports)
 - [Layout Creation](#layout-creation)
   - [Components](#components)
   - [Minimal Layout](#minimal-layout)
 - [Layout Usage](#layout-usage)

# Essential Concepts

The main entity of layout construction in FULE is called a _guide_. Guides represent the boundaries between components, the edges of windows, or invisible things like center-lines for centering content. When you interact with a `Layout`, you will be retrieving the positions of guides or moving them around.

Guides can be positioned absolutely, statically relative to other guides, or suspended elastically between other guides.

At a higher level are _containers_. Containers use guides under the hood to create higher-level constructs such as grids or divided areas. When you create a `Layout` you will probably use containers.

That's pretty-much it as far as usage concepts go. You can dive into [the theory](Theory.md) if you'd like to look a little deeper; otherwise, onward!

# Imports

You only need a single import to use FULE:

```haskell
import FULE
```

# Layout Creation

## Components

The first thing you'll probably want to do in your code is define your visual components -- interactive widgets or static elements -- and provide an instance of FULE's `Component` typeclass for each of them. `Component` tells FULE what the sizing requirements are for your components.

`Component` is a monad-friendly typeclass should you need to interact with stateful data to retrieve sizing information for your components.

Your components will either need to be all of the same type or to have some sort of [wrapper for heterogenous types](https://wiki.haskell.org/Heterogenous_collections) around them in order for FULE to use them.

A convenience implementation of `Component` has been provided for you, for any type, which specifices no height or width requirements. When you define your own instance(s) you should override the default by using the `{-# OVERLAPS #-}` or `{-# OVERLAPPING #-}` pragmas on it (or them).

## Minimal Layout

Next you can create your layout.

Layouts are created at a high level using any of several _containers_. The outer-most container for your layout must be a `Window`; within the `Window` you can add your visual components and other containers. 

Most containers require configuration arguments suitable for their particular functions; `Window` must be provided with:
 - The dimensions of the GUI window that the layout will be displayed in
 - A function to create an (invisible) UI component that you should use to adjust the size of the `Layout` in response to changes in the GUI window's size -- a callback, in other words

For the requirements other containers have, consult [their documentation on Hackage](https://hackage.haskell.org/package/FULE). For some working examples, see the [examples](../examples/) directory.

Once your layout has been defined, you'll build it by passing the `Window` to one of the `layout` or `layoutM` functions[^1]. The result of this will be a tuple of type `(Layout, [ComponentInfo k])`, where `k` is your component type.

The list of `ComponentInfo`s contains your visual components along with meta-information about their situation within the layout, including their display `Bounds`, which can also contain info for clipping, and `RenderGroup`, which can be used for dealing with z-overlaps and rendering updates.

[^1]: The -`M` variants of functions (e.g. `layoutM`) or types (e.g. `GridM`) work in a generic monad so you can do stuff like extract the dimensions of a component using `IO` during the layout process. If you don't need a monad or want to keep things pure you can use the non-`M` variants which work in the `Identity` monad behind the scenes.

# Layout Usage

Now that you have a layout to use, you can wire it to the rest of your application.

User-input should be piped in to your components in the `ComponentInfo`s; an instance of `Functor` has been provided for this wrapper-type so you can easily modify your components within it.

Updates for the `Layout` should be part of the output your components generate. Updates can be applied to the `Layout` using the `reactToChange` or `reactToChanges` functions, giving them movement deltas for the guides that have moved. (Guide values can be retrieved from the `Layout` using the `getGuide` or `getGuides` functions.)

To draw your components, and likely to provide input for them as well, you'll need to know their bounding rectangles within the `Layout`.

The `Bounds` within the `ComponentInfo` for each component contains a set of guide IDs from which a bounding rectangle can be constructed for the component, like so:

```haskell
import FULE

data Rectangle
  = Rect
    { xOf :: Int
    , yOf :: Int
    , widthOf :: Int
    , heightOf :: Int
    }

toRectangle :: Bounds -> Layout -> Rectangle
toRectangle bounds layout =
  Rect left top (right - left) (bottom - top)
  where
    [top, left, right, bottom] = boundingGuidesFor layout bounds
```

In the event that you're concerned about clipping content and are using the `Clipped` container, you can get the clipping rectangles from the `Bounds` as well:

```haskell
toRectangles :: Bounds -> Layout -> [Rectangle]
toRectangles bounds layout =
  case clippingOf bounds of
    Nothing -> [toRectangle bounds layout]
    Just cl -> toRectangle bounds layout : toRectangles cl layout
```

The rectangle at the head of the list returned by this function is the bounds for the component; all the rectangles in the list should be intersected to find the clipped display area. (Don't forget to consider clipping when reacting to input!)

# Conclusion

That should get you started, do let me know if there are any questions or issues!
