module SvgPath exposing (join, m,l,a)
import Util exposing (..)

join = String.join " "

ff = String.fromFloat
fb b = if b then "1" else "0"

m dx dy = strf "m % %" [ff dx, ff dy]
l dx dy = strf "l % %" [ff dx, ff dy]
a rx ry xrot large_arc sweep dx dy = strf "a % % % % % % %" [ff rx, ff ry, ff xrot, fb large_arc, fb sweep, ff dx, ff dy]
