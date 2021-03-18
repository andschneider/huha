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
└── static
```

The layout is based on the [Hugo directory
structure.](https://gohugo.io/getting-started/directory-structure/) 

### content

This is where the markdown files should be.

### layouts

The [mustache](https://mustache.github.io/) template files that are used
to define the structure of and generate the HTML.

As of now, there are two template files that should be defined:

- `main.mustache` - this is the base template that will become the `index.html`
- `category.mustache` - this template is used for individual pages

### static

Things like CSS, JS, and images.

## output

The generated site will be created in a `public` directory. The whole directory,
i.e. `public/`, should be exposed by a web server.
