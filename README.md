# Haskell based invoices

The following tools are used:
* [clay](http://fvisser.nl/clay/) library to generate CSS
* [lucid](https://hackage.haskell.org/package/lucid) library to generate HTML
* [wkhtmltopdf](https://wkhtmltopdf.org/) to generate a PDF from HTML

To generate the first invoice, run

``` shell
$ stack build && stack exec invoice -- oct-2016 | wkhtmltopdf - oct-2016.pdf
```

The `invoice` executable will generate the HTML invoice. `wkhtmltopdf` reads it
from stdin and transforms it into a PDF.
