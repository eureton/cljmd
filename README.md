# cljmd

Clojure implementation of [CommonMark](spec.commonmark.org).

## Installation

### Leiningen

Add the following to `:dependencies` in your `project.clj`:

``` clojure
[org.clojars.eureton/cljmd "0.1.0"]
```

## Usage

Get an AST:

``` clojure
cljmd.core=> (cljmd.ast/from-string "[_awesome_ stuff](http://example.com)")
{:data {:tag :doc}, :children [{:data {:tag :p}, :children [{:data {:tag :a, :destination "http://example.com"}, :children [{:data {:tag :em}, :children [{:data {:tag :txt, :content "awesome"}}]} {:data {:tag :txt, :content " stuff"}}]}]}]}
```

Pretty-print it:

``` clojure
cljmd.core=> (pp)
{:data {:tag :doc},
 :children
 [{:data {:tag :p},
   :children
   [{:data {:tag :a, :destination "http://example.com"},
     :children
     [{:data {:tag :em},
       :children [{:data {:tag :txt, :content "awesome"}}]}
      {:data {:tag :txt, :content " stuff"}}]}]}]}
```

Even better, pretty-print it using the AST printer:

``` clojure
cljmd.core=> (ppp)
<doc>
  <p>
    <a destination="http://example.com">
      <em>awesome</em>
      <txt> stuff</txt>
    </a>
  </p>
</doc>
```

Get HTML:

``` clojure
cljmd.core=> (cljmd.render/from-string "[_awesome_ stuff](http://example.com)")
"<p><a href=\"http://example.com\"><em>awesome</em> stuff</a></p>\n"
```

## License

[MIT License](https://github.com/eureton/squirrel/blob/master/LICENSE)
