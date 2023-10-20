# Introduction

GUIs are commonly defined and stored as a tree of objects. Updating the GUI means changing elements of this tree.

Updating trees where there's mutable data is easy, but updating nested immutable data-structures can be onerous and inefficient (though conveniences such as zippers and lenses can make it easier).

While GUIs in FULE are initially defined using a tree of function calls, the result is a flat layout data-structure and a list of UI components (along with meta-information) that should be easier to work with in an immutable language.

In this document we'll go over the math of how this works to create a responsive layout, which will require elementary knowledge of linear algebra (i.e. matrix multiplication).

# Guides

The main entity of layout construction in FULE is called a _guide_. Guides represent the boundaries between components, the edges of windows, or invisible things like center-lines for centering content.

Each guide has a value associated with it, and these values are kept in a vector (in the mathematical sense).

For example, a bare window has four guides associated with it: the _top_ and _left_ guides both have a value of zero, representing the origin of the window, and the _right_ and _bottom_ guides have values like e.g. 800 and 600, respecitively, representing the width and height of the window in pixels.

The layout-vector for this example is one with four rows, one for each guide:

$$
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  600
  \end{matrix}
\right]
$$

If we were to divide the window into two parts content-wise, a top and a bottom, with the bottom portion being 100 pixels in height, we would add another value to the layout-vector to represent the dividing guide, giving it a value of 500 -- one-hundred pixels above the guide for the bottom of the window:

$$
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  600\\
  500
  \end{matrix}
\right]
$$

When one wants to obtain the boundaries of a visual component, one just queries the layout-vector for the required guides' values.

# Dependencies

When the value of a guide in the vector changes, e.g. in response to a window resize, we'll often want other guides to be repositioned relative to them as well, to maintain the integrity of the layout.

FULE encodes this sort of relationship using _dependency matrices_, of which there are two. We will discuss each one in turn.

## Plastic Dependencies

The plastic dependency matrix specifies a set of rigid, direct relationships between guides, causing changes to one guide to propagate to the others plastically linked to it.

Continuing with our split-window example from above: say we wanted the bottom portion of the window to always be 100 pixels in height, even after the window was resized. Any changes to the value of the guide for the bottom of the window then would need to be applied to the value for the dividing guide as well, to maintain a difference of 100 pixels between them.

The plastic dependency matrix encoding this linkage would look like this:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 0\\
  0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
$$

This is essentially an identity matrix, but note the extra $1$ in the bottom row: this non-zero entry links the division guide (represented by the bottom row) to the bottom window guide (represented by the next-from-bottom row) so that when an update occurs to the window guide, it will be applied to the division guide as well.

If the window were resized to be 25 pixels shorter, the propagation of this change to the division guide would happen like so:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 0\\
  0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  -25\\
  0
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  -25\\
  -25
  \end{matrix}
\right]
$$

And the update to the layout vector would then happen like this:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 0\\
  0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  -25\\
  0
  \end{matrix}
\right]
+
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  600\\
  500
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  575\\
  475
  \end{matrix}
\right]
$$

### Symmetries

Note that the change-propagation in this example only goes one-way: since the entry across the diagonal from the extra $1$ is a $0$, changes to the division guide will not be propagated to the bottom window guide, only the other way around -- their dependency relationship is _asymmetric_.

To demonstrate a _symmetric_ relationship, let's add yet another guide to the layout: this time we'll add one just 10 pixels above the division guide to create the top and bottom of a _resize bar_ which a user could drag to change the sizes of the two larger areas of the window simultaneously.

Our layout-vector now looks like this:

$$
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  600\\
  500\\
  490
  \end{matrix}
\right]
$$

For the behavior of this resize bar to make sense, any change to either one of the resize bar's guides must be applied to the other, otherwise the bar would expand and contract in size.

The plasticity matrix therefore should link them together with a $1$ on both sides of the diagonal in the row and column for each bar guide:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
$$

Now a direct update to either one of the guides will affect the other:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  0\\
  -15\\
  0
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  0\\
  -15\\
  -15
  \end{matrix}
\right]
$$

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  0\\
  0\\
  -15
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  0\\
  -15\\
  -15
  \end{matrix}
\right]
$$

The relationship of these two guides is thus symmetric.

### Propagation

Relationships between the guides specified in this way allows us to propagate changes from one guide to another automatically, but only for direct linkages; transitive links won't be realized the way things are currently set up because there's nothing linking the linkages.

Say the user resized the window, like before, with the latest plasticity matrix:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  -25\\
  0\\
  0
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  -25\\
  -25\\
  0
  \end{matrix}
\right]
$$

The link between the bottom window guide and the division guide causes the change to propagate to the division guide as before, preserving their 100 pixel difference, but the change does not continue on to affect the other guide of the resize bar. It should, however, because the other resize bar guide is in a relationship with the division guide and should move the same amount that it does.

This defficiency is due to the fact that the plasticity matrix entry which would link the window bottom to the second resize bar guide -- bottom row, fourth column -- is a $0$. We need some way to link these relationships together, which would turn this entry into a $1$.

If we look at both of the propagations discussed thus far, we have one propagation from the bottom window guide to the division guide, caused by an application of the plasticity matrix, and a propagation from the division guide to the other resize bar guide, also caused by an application of the matrix.

Each application results in one propagation -- from window to division and from division to bar. What, then, if we were to apply the pasticity matrix _twice_ -- would that create a transitive link?

Applying the matrix twice is equivalent to squaring it, then applying it, so let's see what happens when we square it:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 0 & 1 & 1
  \end{matrix}
\right] ^{2} =
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 2 & 2 & 2\\
  0 & 0 & 0 & 1 & 2 & 2\\
  \end{matrix}
\right]
$$

The entry to link the guides that we'd wanted to be linked _is_ a $1$ in the squared matrix, like we'd desired, but other entries have been changed from a $1$ to a $2$, which we didn't want. We can remediate this by setting all non-zero entries of the matrix to $1$ after the square.

Successive applications of the plasticity matrix serve to propagate the linkage of the guides (if we set things to $1$ after), so a general propagation algorithm implementation would involve applying apply the matrix $n-1$ times for an $n$-by-$n$ matrix, just to make sure we cover all the linkages.

Alternatively, we could iteratively square the matrix (and set things to $1$) until there were no more changes upon iteration, and that should take care of linking everything together.

We'll call this iterative propagation operation $prop$:

$$
prop\left(
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
\right) =
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 1 & 1 & 1\\
  \end{matrix}
\right]
$$

The update procedure now becomes:

$$
prop\left(
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0 & 0\\
  0 & 0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 1 & 1\\
  0 & 0 & 0 & 0 & 1 & 1
  \end{matrix}
\right]
\right)
\left[
  \begin{matrix}
  0\\
  0\\
  0\\
  -25\\
  0\\
  0
  \end{matrix}
\right]
+
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  600\\
  500\\
  490
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  575\\
  475\\
  465
  \end{matrix}
\right]
$$

which updates all the guides we'd wanted to change.

## Elastic Dependencies

Plastic dependencies give us rigid links between guides; _elastic_ dependencies give us _stretchy_ links.

Whereas plastic dependencies let us specify an offset between guides in **pixels**, elastic dependencies let us specify offsets in **percents**.

Suppose we wanted to divide a window into two vertical halves and have each half always occupy 50% of the window. There's no set pixel value we can use to have this division always remain in the center; we have to make it move relative to the two window sides when an update occurs, splitting the difference of their movements.

If we recycle our 800x600 window for this new example, the layout vector for the window will have a vertical guide (at 400 pixels) between the two vertical window sides:

$$
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  600\\
  400\\
  \end{matrix}
\right]
$$

And the elasticity matrix to split the difference between the two sides' movements for this guide when an update takes place will be:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 0\\
  0 & 0.5 & 0.5 & 0 & 1\\
  \end{matrix}
\right]
$$

The two $0.5$ entries in this matrix represent the amount of influence each of the window-side guides has on the division guide; since it's halfway between them they each exert half of the influence on it.

If a user were to expand the right side of the window by 50 pixels then we'd have an update process of:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 0\\
  0 & 0.5 & 0.5 & 0 & 1\\
  \end{matrix}
\right]
\left[
  \begin{matrix}
  0\\
  0\\
  50\\
  0\\
  0\\
  \end{matrix}
\right]
+
\left[
  \begin{matrix}
  0\\
  0\\
  800\\
  600\\
  400\\
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  0\\
  0\\
  850\\
  600\\
  425\\
  \end{matrix}
\right]
$$

which keeps the division guide in the center of the window.

Now suppose we were to alter this deisgn to have the vertical portions occupy 25% of the window on the left and 75% on the right. How would this change the elasticity matrix?

The $0.5$ entries representing the 50%s would have to change, but possibly not how you think.

Remember that these values represent the influence the window-side guides have on the division guide. If we change the division guide to be closer to one of the sides, then that side will have more influence; moving the guide to be 25% of the way from one side means that that side's guide will have 75% of the influence on the division guide.

The values for these entries in the elasticity matrix then are one minus the percent distance the dependent guide is from the influencing guide, so the new elasticity matrix would be:

$$
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 0\\
  0 & (1-0.25) & (1-0.75) & 0 & 1\\
  \end{matrix}
\right] =
\left[
  \begin{matrix}
  1 & 0 & 0 & 0 & 0\\
  0 & 1 & 0 & 0 & 0\\
  0 & 0 & 1 & 0 & 0\\
  0 & 0 & 0 & 1 & 0\\
  0 & 0.75 & 0.25 & 0 & 1\\
  \end{matrix}
\right]
$$

# Update Cycle

We have two matrices that need to be applied to produce an update to the layout, and we also have propagation to be concerned with.

To forgo some explanation, the following is the current, working update procedure (as determined by providence, reason, and experimentation):

Given a plasticity matrix $P$, an elasticity matrix $E$, an update vector $U$, and the layout-guide value vector $L_n$, the procedure to get the next vector of layout-guide values $L_{n+1}$ is:

$$
\begin{align}
D & = prop(present(E) \star P) \\
T & = D \star E \star D \\
L_{n+1} & = T U + L_n
\end{align}
$$

where:

$$
present(M) =
\forall{e}\in{M}: e \leftarrow
\left\lbrace
\begin{array}{ll}
1 & e \neq 0 \\
0 & \text{otherwise}
\end{array}
\right.
$$

The $\star$ is a new operation: it is a modified matrix multiplication operation; whereas in normal matrix multiplication the elements are multiplied piece-wise and then summed, in $\star$ the summation is replaced with the product of the non-zero elements of the piece-wise multiplication.

Using $\star$ within $prop$ replaces having to reset the entries to $1$ each iteration, and it also helps propagate the elastic coefficients more correctly in the calculation of $T$[^1].

Since $P$ and $E$ don't change each time an update is applied we can pre-compute the combined transformation matrix $T$ just once for efficiency's sake and use it for every update cycle; any changes to the composition of the layout (and thus to $P$ and $E$) will require rebuilding $T$ though.

[^1]: Sadly things still aren't propagating correctly when deeply nesting elastic containers, and this is still a work in progress.

# Guide Constraints

FULE also offers a way to constrain the movement of one guide relative to another, but as this feature is yet more experimental I'll not cover the theory at this time. The above discussion should give you a good basis to understand what's going on for that though should you wish to dive into the code.

# Analysis

Rather than having a resize event that propagates throughout a GUI tree to notify all the components it affects of a size change, we keep the layout information in a flat data-structure and can pre-compute this propagation only once beforehand.

Depending on how you're managing your layout you may need to reconstruct the bounds of the components from the guides every frame, but that shouldn't be too much of a hassle with a flat list of components.
