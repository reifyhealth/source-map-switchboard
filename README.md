## source-map-switchboard

source-map-switchboard allows you to plug into your javascript source, source-maps, and original source files
to easily perform source-map lookups.

### Basic Usage

The `single` command takes a string formatted as <filepath>:<line>:<col>.
It will attempt to find a sourcemap with the same filepath with a `.map` suffix.
It prints an edn hash-map that contains an `:original-file-content` key that maps
to a hash-map with line number keys and line content values.

TODO: Allow a command line options to allow output specification of output format
(edn is nice, but it'd be useful if the output could be formatted and presented nicely).


``` bash
source-map-switchboard single ~/code/source-map-switchboard/out/cljs/core.js:10:0

{:js-filepath
 "/Users/tomjkidd/code/source-map-switchboard/out/cljs/core.js",
 :source-map-filepath
 "/Users/tomjkidd/code/source-map-switchboard/out/cljs/core.js.map",
 :dir "/Users/tomjkidd/code/source-map-switchboard/out/cljs",
 :js-position {:line 10, :column 0},
 :original-position
 {:source "core.cljs", :line 19, :column 0, :name nil},
 :original-filepath
 "/Users/tomjkidd/code/source-map-switchboard/out/cljs/core.cljs",
 :original-file-content
 {14 "            [goog.array :as garray]\n",
  15 "            [goog.Uri])\n",
  16 "  (:import [goog.string StringBuffer]))\n",
  17 "\n",
  18 ";; next line is auto-generated by the build-script - Do not edit!\n",
  19 "(def *clojurescript-version* \"1.9.946\")\n",
  20 "\n",
  21 ";; Setting of these Vars is in ClojureScript code is associated with intrinsics\n",
  22 ";; that affect compilation state, but otherwise turn into no-ops in the emitted\n",
  23 ";; JavaScript.\n",
  24 "\n"}}
```

### Motivation

I wanted a tool that could do quick source-map lookups for stacktraces I was getting from minified clojurescript
source code. I encountered [sourcemap-lookup](https://github.com/kkoch986/sourcemap-lookup), and it works great
for single file lookups, but after reading through documentation, it became clear to me that supporting multiple
lookups in a stacktrace use case would require some tweaks. Because lumo allows me to work with npm, but write
new code in clojurescript, I decided to rework some things and build command line interface tool that would be
able to grow if I have more needs in the future.

TODO: The stacktrace functionality is a little rigid. One of the factors is that typically you have a list of files,
which may reflect the actual file structure of a project, but this is not always the case. For this reason, I
have solved for allowing you to provide a couple of configuration params that are suited toward how clojurescript
projects use the closure compiler. It'd be nice to work some of these things to be more general, but figured it is
better to make some progress, and tweak it later.

### Installation

``` bash
lumo --classpath src build.cljs
npm install -g
source-map-switchboard single ~/code/source-map-switchboard/out/cljs/core.js:10:0
```

### License

MIT

### Credits

[sourcemap-lookup](https://github.com/kkoch986/sourcemap-lookup) provided a great starting point, and showed
how to provide pleasant console output. It also helped me understand how to use the underlying source-map
library.