

dup f a = f a a

over f a b = f b a b

swap f a b = f b a

rot f a b c = f c a b

-- reverse function application:
-- instead of f(a), you have (a)f
(¤) = flip ($) -- ($) is regular function application

-- 

sqr = dup (*)

increment = 1 (¤) (+)

