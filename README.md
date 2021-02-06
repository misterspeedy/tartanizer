# Tartanizer

This is a script which can generate a bitmap of a tartan ('plaid') given a 'thread count'.

A tartan thread count is a specification of how many threads of each color are used for both the warp (vertical) and weft (horizontal) threads.

For example "K4 R24 K24 Y4" corresponds to 4 black threads, 24 red threads, 24 black threads, 4 yellow threads.  The script can accept thread counts in these forms:

- Single character color names - e.g. "K4 R24 K24 Y4"
- Standard X11 color names - e.g. "Black4 Red24 Black24 Yellow24"
- Hex RGB values - e.g. "#000000#4 #FF0000#4 #000000#24 #FFFF00#4

See the examples at the bottom of the script for usage of the thread count parser and renderer.
