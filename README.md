# gal

A simple photo-gallery creation webapp.

Allows one to upload some photos and comments, and generates a static webpage
for the corresponding photo gallery.

## Installation

``` 
opam pin add https://github.com/Armael/gal.git
```

## Usage

```
$ gal --help
NAME
       gal - A simple gallery creation webapp

SYNOPSIS
       gal [OPTION]... CONTENT_DIR

ARGUMENTS
       CONTENT_DIR (required)
           Directory where to store (and serve from) the generated content.

OPTIONS
       --debug
           Enable debugging.

       --no-serve-static
           Do not serve static content. If this option is enabled, you need a
           separate web server to serve the static content in CONTENT_DIR.

       -p PORT, --port=PORT (absent=3000)
           Port where to listen to.

       --password=PASSWORD
           Authentication password. The username is "admin". If this option is
           not provided, then there is no authentication.
```
