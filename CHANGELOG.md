# Changelog for `FULE`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.3.1.1 - 2024-05-10
 - Simplified update procedure

## 0.3.1 - 2024-02-16
 - Added `Reactor` type and `Reaction` typeclass for heterogeneous components
 - Updated Usage documentation

## 0.3.0 META - 2024-01-09
 - Changed dependency bounds to make the build on Hackage work.

## 0.3.0 - 2023-12-15
 - Made `Sized`'s `sized` function's size a single argument
 - Made the `Sized` container properly override the inherent size of content.
 - Made Window size a single argument
 - Corrected bounds on dependencies
 - Updated documentation
 - Reorganized (and split) modules

## 0.2.0.3 - 2023-11-16
 - Tweaked documentation for release to Hackage

## 0.2.0.2 - 2023-10-27
 - Fixed constraints
 - Updated documentation

## 0.2.0.1 - 2023-10-27
 - Fully corrected the construction of the transform matrix in the layout `build` function
 - Updated the theory documentation

## 0.2.0 - 2023-10-20
 - Altered construction of the transform matrix in the layout `build` function to fix some issues
 - Updated the theory documentation to describe the new transform matrix
 - Now applying the full transform matrix instead of just the propagated matrix to the constraint matrices in the layout `build` function
 - Changed the grid container API to accept a tuple rather than two separate arguments for specifying the number of rows and columns the grid should have.

## 0.1.0 - 2023-09-28
Initial release.

