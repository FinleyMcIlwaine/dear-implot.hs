# How to contribute

- Look at [https://github.com/epezent/implot/blob/master/implot.h](https://github.com/epezent/implot/blob/master/implot.h) for things you
  need
- wrap them in `src/DearImGui/Raw/Plot.hs` like shown there
- write a thin user-facing wrapper in `src/DearImGui/Plot.hs` with normal
  Haskell-Types (and not things like `CInt`, `Ptr Foo` etc.)
- submit a PR

# Things to consider

To have things work the version of `dear-implot` is very tightly bound to the
soucecode-versions of `imgui` used in `dear-imgui` and the corresponding
`implot` used here.
