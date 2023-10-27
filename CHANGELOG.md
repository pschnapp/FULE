# Changelog for `FULE`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased
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

