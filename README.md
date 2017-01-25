Unicode support library
=======================

This [**Erlang**][1] library provides access to character properties defined in the
Unicode Character Database and implements following algorithms specified
in the Unicode 9.0.0 standard:

  * **Default Case Algorithms** specified in [The Unicode Standard][2] Section 3.13
    - *Default Case Conversion*
    - *Default Case Folding*
    - *Default Caseless Matching*

  * **Unicode normalization** specified in The Unicode Standard [Annex #15][3]
    - *Normalization Form D*
    - *Normalization Form KD*
    - *Normalization Form C*
    - *Normalization Form KC*

  * **Unicode Text Segmentation** specified in The Unicode Standard [Annex #29][4]
    - *Grapheme Cluster Boundaries*
    - *Word Boundaries*
    - *Sentence Boundaries*

  * **Unicode Line Breaking Algorithm** specified in The Unicode Standard [Annex #14][5]

  * **Unicode Bidirectional Algorithm** specified in The Unicode Standard [Annex #9][6]


Build
-----

    $ rebar3 compile


Documentation
-------------
Run `rebar3 edoc` and open generated doc/index.html file.


[1]: http://www.erlang.org/
[2]: http://www.unicode.org/versions/Unicode9.0.0/
[3]: http://www.unicode.org/reports/tr15/
[4]: http://www.unicode.org/reports/tr29/
[5]: http://www.unicode.org/reports/tr14/
[6]: http://www.unicode.org/reports/tr9/
