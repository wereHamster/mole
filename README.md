## Mole

A glorified string replacement tool. For a very specific purpose.
That purpose being to compile and optimize a static website (or a single-page application). Mole
inspects all files, builds a complete dependency tree, minifies and
compresses the files, adds fingerprints and writes the result to
a directory.

Mole includes a web server to serve the files directly (great
during development), and a filesystem watcher which detects changes
in source files and recompiles the website (also great during development).

The goal is that when you have a static website with standard HTML5, CSS,
JavaScript files, images, font files etc, that it can produce an optimized
version that you can copy to a web server. That all should work out of the
box, without having to configure the tool. These transformations (optimize,
minimize, fingerprint etc) should be part of the standard toolchain.
Unfortunately, the existing tools (gulp, webpack, grunt, browserify etc) require
users to download and configure lots of different plugins. This all is overhead
that we simply should not have.


### Usage

Use the `build` command to compile the website and write into an output directory. In the following
example all HTML files in the `public`, `assets` and `tmp/js` directories
will be used as entry points and the result will be written into `output/`.

```sh
$ mole --paths public:assets:tmp/js build output/
$ find output/ -type f
output/index.html
output/BmzSvw4DX-page-WX4ihgSer4F9XRjSGLlfPlDJZEZB.js
output/cBqdRpkR4-2-double-bounce-3MLChiQbj6TT6muzK.css
output/FJbIHYc2l-icons-NeCQFcz0RY8Q0xL1uG0bZYFO5UY.ttf
output/Jsk8sOmoJ-icons-fiGIBQamc3AUCdMQottjO3HrxSA.svg
...
```

To serve the files instead use the `serve` command in place of `build`. The
server will listen on port 8000. It will also watch files in the source
directories and when they change recompile all dependent files.

```sh
$ mole --paths public:assets:tmp/js serve
Listening on http://0.0.0.0:8000/
...
11:09:38 [ index.html ] Rebuilding because assets/index.html was modified
```



### Limitations

Mole is intentionally limited in what forms of transformation it can do.
One file in, one file out. One reference is replaced with one other reference.
It can't expand references (e.g. replace one script
tag with multiple script tags in a HTML file) or the opposite, compile bundles
from multiple files
(e.g. replace multiple script tags with a single script tag referencing the compiled bundle). With the
adoption of HTML2 bundling becomes largely irrelevant anyways.

There are cases where bundling is useful, but those require more complex
transformation of the source files. For example when using sprites the tool has to
add additional CSS attributes to the selector (`background-position`).
In those cases you are encouraged to use an existing build tool (e.g. sass/compass for sprites).


### Building the dependency tree

To build the dependency tree, Mole simulates a web browser. It starts with an
entry point, usually `index.html`. The HTML file references other files
(other HTML files through `<a href=X ..`, images through `<img src=X ...`,
CSS files thorough `<link src=X ...` etc.). These files in turn reference
yet other files (e.g. CSS files can use `url(x)` to refer to images).

So far we've been able infer arbitrary references from the standard HTML
and CSS syntax. However, in case of JavaScript there is none! `import X from "Y";`
can only be used to refer to other JavaScript files, there is no standard
to refer to other types of files, for example if you need the URL to an image in your
JavaScript code.

Many build tools (browserify, webpack) reuse the same `import .. from ..` syntax, but then must apply
complex source code transformations to replace the parts with standard
JavaScript syntax. That in turn requires a full JavaScript parser, which makes
these tools slow.

For these cases Mole supports a different syntax that is completely transparent
to the JavaScript language and thus your toolchain:
`let imageUrl = __assetUrl("images/kitten.png");`. It's a plain JavaScript string, but
wrapped with a function call so that it can be detected by Mole. The function name is chosen
to be unlikely to clash with any existing uses. It's very fast to
detect these fragments in a source file, and that keeps processing time low.

Mole does not remove the function call, you need to define that function somewhere in your project.
If you structure your project well, you can make it so that it works with and without mole in
the toolchain. That way you really can treat mole purely as an optimization step.


### Optimizations

Mole automatically fingerprints all files. Furthermore it applies the following
optimizations (checked when implemented, otherwise implementation is either
planed or in progress):

 - **HTML**
   - [ ] Strip whitespace
   - [ ] Clean up and normalize attributes
 - **CSS**
   - [ ] Strip whitespace
   - [ ] Inline small images
   - [ ] Prefix attributes for compatibility with older browsers (autoprefixer)
 - **JavaScript**
   - [ ] Minify/Uglify
 - **Images**
   - [x] Recompress (kraken.io)

### Additional toolchain

If your source files are not in standard HTML, CSS, JS then you must process
them with appropriate tools. Tools like gulp, grunt or even browserify or webpack
can be used for that. But note that they don't have to be configured to optimize
the output, just to emit standard HTML, CSS and JS.


## FAQ

### What's the difference between Mole an webpack?

Webpack is missing one crucial piece, you can't use a HTML file as the entry point!
Other than that, webpack is great, I do recommend it as a build tool which produces
files which you reference from your HTML files. Then you can process the entry HTML
file through Mole to generate or directly serve the site.
