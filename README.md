# huha

A bare bones static site generator written in Haskell.

## about

This project is an attempt to create a minimal static site generator, meaning
transforming markdown files into html files to be viewed in your browser. The
motivation behind this was to give me an easy and custom way to create a
website.

I've used [Hugo](https://gohugo.io/) before, which is a nice product, but it has
a lot of options and features. I was after something simpler, and, as will be
described later, a little specific.

It's worth mentioning briefly that the name of this project, `huha`, is a mash
up of `hugo` and `haskell`.

### libraries

This project uses [commonmark-hs](https://github.com/jgm/commonmark-hs) to parse
the markdown files and generate the html. There were a few libraries that I
could have chosen, but I went with commonmark-hs because it is being actively
developed and was written in pure Haskell. Additionally, the developer is the
same person who wrote and maintains [pandoc](https://pandoc.org), which is a
tool I've used many times over the years. When I realized he wrote commonmark-hs
(and also that pandoc is written in Haskell) I felt compelled to use it.

Another central library that is used in this project is
[stache](https://github.com/stackbuilders/stache). This library enables quick
and easy templating using the [mustache](https://mustache.github.io/)
specification. I'd never heard of mustache before, but I didn't have any issues
using the template language.

With these two libraries, the flow of building a static site is as follows:

1. Read in markdown files from disk
2. Parse and convert to html using _commonmark-hs_
3. Read in and parse the mustache templates
4. Inject the converted html into the template using _stache_
5. Save to disk as a html file

Overall, it is a fairly straight forward workflow. However, something I did not
expect when I started was how strange IO felt in Haskell. Since most of the
steps outlined above involve some sort of IO, I spent a lot of time banging my
head against the wall with this concept. Especially when I started the project,
and had never seen a Monad before, I was quite confused.

The `Parser.hs` file is where I started. There are some functions in there that
I definitely think could be combined or removed, mainly around handling the
Either type. This file handles step 2 from above, and it made me realize that
one of the hard things about using Haskell is the seemingly lack of good
documentation and examples. In order to use the commonmark-hs library
effectively I ended up basically reading the GitHub and looking for examples,
including going through all the GitHub issues, which was semi-fruitful.

An additional library I used was the
[fsutils](https://github.com/Raynes/fsutils) library. I only used one function,
the `copyDir` function, but it's worth pointing out because I was unable to use
this library with Stack/Cabal. When I tried to add it as a dependency, it threw
a bunch of errors about the GHC version. I suppose this makes sense since the
library hasn't been updated in 9 years. To get around this, I vendored it into
my source as the `Fsutils.hs` file.

Finally, the last major library I used was
[optparse-applicative](https://github.com/pcapriotti/optparse-applicative). I
threw this in at the end to create a little CLI (because I like making CLIs)
and I actually really liked it. Especially after the 3rd homework assignment I
felt comfortable using it and understanding it. I think it's a really powerful
library and compares well to Python's `argparse`, which is probably my favorite
CLI library. However, I think my experience with this library will make me pick
up Haskell again at some point when I have a little CLI to make.

### specifics

I started out this project with the intention of building a general static site
generator. It quickly devolved into a static site generator for my personal
notes. I've been taking notes related to programming in a single markdown file
for a couple years now, and I thought it would be cool to view them in a
browser.

The point where it got super specific was when I decided it would be a good idea
to break up the single large file based on the **tag** given to the note, and
create individual html files for each tag. The format of the file is given
below. Basically, I decided to parse each note (which is in between the two
`------` markers, and then use the tag in the header (the word inside `[[ ]]`)
as the name of the html file.

```markdown
------ 

## 20210319 - [[readme]] - readme example

notes...

------
```

Internally, the program uses patterns to split and extract the header
information. They are called _separator_ and _header_, respectively. They can be
specified at runtime as described in the usage section below.

I admit this is a weird way to split up a file. However one of the alternatives
that is commonly used in static site generators is the concept of "front matter"
. (See [hugo's docs](https://gohugo.io/content-management/front-matter/)
for more info.) I didn't have many files that use front matter, so I decided to
go another route.

## building

Run `stack build` or `make build`. Using `make` will create a `build` directory
and copy the binary into it. I found this easier to use than using `stack run`
when I was testing out the CLI arguments.

## usage

Now for the fun part!

Once the binary is built, it can be used to generate a static site. To do this,
the first step is ensuring the correct input files are present. See the
following section about directory structure for specifics.

The files given in the `example` directory should work fine for now though. So,
to build these run the following command:

`./build/huha --input ./example --output ./output --file notes-cs.md`

> This assumes you are building from the base of the project using the binary created by the `make build` command.
 
> There will be no message displayed if things worked correctly.

It should have generated a site inside the `./output/public` directory. To view
the files in a browser simply use any web server to serve the directory. I used
Python for testing, so for example, after cd-ing into the `public` directory, I
would run `python3 -m http.server`.

To view the files, now go to `localhost:8000` in your browser.

For more commands, see the help text from the CLI. I added options to change the
separator and header pattern strings, but I have a feeling that changing these
too much might result in some crashes (the parsing is using simple string prefix
matching, so keep that in mind).

If you'd like a reference of the output I got, you can check out a hosted
version [here](https://goofy-fermat-7b6850.netlify.app). I'll leave that up
until at the least the end of March.

For more information on what was generated, see the output section below.

### help

Run `huha --help` to see more detailed information:

```bash
huha - static site generator

Usage: huha [-i|--input PATH] [-o|--output PATH] (-f|--file FILE) 
            [-h|--header ARG] [-s|--separator ARG] [--dry]
  Build a static site from a single file.

Available options:
  -i,--input PATH          Input directory to build (default: ".")
  -o,--output PATH         Location to create a 'public' directory and build
                           site to (default: ".")
  -f,--file FILE           Filename of markdown file. It should be in the
                           'content' directory.
  -h,--header ARG          Header pattern to use in searching the begning of
                           lines for headers. (default: "## 2")
  -s,--separator ARG       Separator pattern to use to split up the
                           file. (default: "------")
  --dry                    Dry run the build. The output directory structure
                           will still be created (if needed).
  -h,--help                Show this help text
```

## directory structure

The directory structure that huha expects as input is the following:

```text
.
├── content
├── layouts
└── static
```

The layout is based on
the [Hugo directory structure.](https://gohugo.io/getting-started/directory-structure/)

### content

This is where the markdown files should be.

### layouts

The [mustache](https://mustache.github.io/) template files that are used to
define the structure of and generate the HTML.

As of now, there are two template files that should be defined:

- `main.mustache` - this is the base template that will become the `index.html`
- `category.mustache` - this template is used for individual pages

### static

Things like CSS, JS, and images.

In the example site, I am using [prism.js](https://prismjs.com) to provide
syntax highlighting for the code snippets.

## output

The generated site will be created in a `public` directory. The whole directory,
i.e. `public/`, should be exposed by a web server.

Inside the `public` directory should be the following:

```text
public
├── index.html
├── notes
└── static
```

- The `notes` directory holds all the individual pages.
- The `static` directory is a copy of what was in the `static` directory in the
  input folder

## testing

There are some pretty minimal and poorly written tests in the `test` directory.
I struggled to figure out a good way to test the IO things.

I did use GitHub Actions to run the tests on every push though, which was a
nice way to make sure I didn't majorly break things. The setup for this is in
the `.github/workflows/build.yml` file.
