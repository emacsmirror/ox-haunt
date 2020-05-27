# ox-haunt

This library implements an HTML back-end for the Org generic exporter, producing
output appropriate for [Haunt's](https://dthompson.us/projects/haunt.html)
`html-reader`.

To use this exporter, add the following to your Emacs configuration:

```elisp
(with-eval-after-load 'ox
  (require 'ox-haunt))
```

As of version 0.1, only one workflow is supported. The "haunt base directory" is
set by adding `#+HAUNT_BASE_DIR`, followed by the path, to your document, or by
setting `ox-haunt-base-dir`. When the base directory is set, `C-c C-e s h` will
export the Org buffer to a file in the 'posts/' subdirectory of the haunt base
directory.

## Users

- [Jakob's Personal Webpage](http://jakob.space/)
- [bendersteed.tech](https://bendersteed.tech/posts/migrating-from-hugo-to-haunt.html) (thanks for the kind comments!)

Would you like your blog listed here? Send a patch!
