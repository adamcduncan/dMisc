# dMisc
A collection of helper functions for working with financial data.

  This collection of functions is basically a set of tools that I have created
  to help with data analysis involving financial data. Most of these are just small annoyances
  that I have resolved with short functions that speed development of larger applications. For
  example, you might want to create a cumulative wealth index from return data on the fly.
  makeIndex() aids in this task. There are many xts helpers like xtsApply(), which is a generic
  apply function for xts objects. Things like making a multi-variable xts object from a list of
  tickers can be handled by tickersToXTS(). In many ways, this package is like the hMisc package
  which I imagine was created to resolve the many issues they encountered in data to day R life.
