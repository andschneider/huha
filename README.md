# huha

A bare bones static site generator written in Haskell.

![Build](https://github.com/andschneider/huha/workflows/Build/badge.svg)

## usage

> WIP

## directory structure

The directory structure that huha expects as input is the following:

```text
.
├── content
├── layouts
├── public
└── static
```

The layout is based on the [Hugo directory
structure.](https://gohugo.io/getting-started/directory-structure/) 

### content

This is where the markdown files should be.

### layouts

The [{{ mustache }}](https://mustache.github.io/) template files that are used
to generate the HTML.

### public

The generated site is built here. The whole directory, i.e. `public/`, should
be exposed to a web server.

### static

Things like CSS, JS, and images.
