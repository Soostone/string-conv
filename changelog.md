0.1.2
=====
* Fixed bug where String <-> ByteString conversion would trim characters to 8 bits. Now it goes through conversion to Text first.

0.1.1
=====
* Fixed bug where decoding text from bytestrings would always decode
  leniently, even if Strict was used.

0.1
===

* Initial release
