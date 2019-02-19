app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(tabs = "plot")
app$snapshot()
app$setInputs(`sd-Ah` = 1.3)
app$snapshot()
app$setInputs(tabs = "table")
app$uploadFile(`datafile-file` = "braunerde.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(tabs = "plot")
