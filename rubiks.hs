-- A lot of this will be much easier to write/check/edit with a 
--  similarly coloured Cube in hand and having memorised the order
--  of the stickers and faces


-- lookout for todos! 


data Sticker = White | Orange | Green | Red | Blue | Yellow deriving Show

-- Order of stickers on each face: 
-- 1 2 3
-- 4 5 6
-- 7 8 9

-- A face has 9 stickers
data Face = Face (Sticker, Sticker, Sticker, Sticker, Sticker, Sticker, Sticker, Sticker, Sticker) 

instance Show Face where
 show (Face (s1,s2,s3,s4,s5,s6,s7,s8,s9)) = (show s1) ++ "\t" ++ (show s2) ++ "\t" ++ (show s3) ++ "\n" 
                                         ++ (show s4) ++ "\t" ++ (show s5) ++ "\t" ++ (show s6) ++ "\n" 
                                         ++ (show s7) ++ "\t" ++ (show s8) ++ "\t" ++ (show s9) ++ "\n"

-- Order of faces: Top(1), Left(2), Front(3), Right(4), Back(5), Bottom(6)
-- With White on top and Green in front, 
--  the order of faces (in colour) should be: White, Orange, Green, Red, Blue, Yellow

-- A cube has 6 faces
data Cube = Cube (Face, Face, Face, Face, Face, Face) deriving Show

solvedFace :: Sticker -> Face
solvedFace colour = Face (colour, colour, colour, colour, colour, colour, colour, colour, colour)

solvedCube = Cube (solvedFace White, solvedFace Orange, solvedFace Green, solvedFace Red, solvedFace Blue, solvedFace Yellow)


-- On the Rubik's Cube, doing a basic transformation thrice is like doing its inverse 
--  because the basic transformations have order 4
inverse4 :: (a -> a) -> a -> a
inverse4 f = f . f . f

----------------
-- Cube turns --
----------------

-- Note: Cube turn names are usually abbreviated by uppercase letters,
--  but unfortunately haskell function names must begin in the lowercase.

-- Turn right face a quarter turn clockwise
r :: Cube -> Cube 
r (Cube (Face (s11,s12,s13,s14,s15,s16,s17,s18,s19),
        f2,
        Face (s31,s32,s33,s34,s35,s36,s37,s38,s39),
        f4,
        Face (s51,s52,s53,s54,s55,s56,s57,s58,s59),
        Face (s61,s62,s63,s64,s65,s66,s67,s68,s69))) = Cube (Face (s11,s12,s33,s14,s15,s36,s17,s18,s39),
                                                            f2,
                                                            Face (s31,s32,s63,s34,s35,s66,s37,s38,s69),
                                                            (clockwise f4),
                                                            Face (s19,s52,s53,s16,s55,s56,s13,s58,s59),
                                                            Face (s61,s62,s57,s64,s65,s54,s67,s68,s51))

-- Turn right face a quarter turn anti-clockwise
r' :: Cube -> Cube
r' = inverse4 r

-- Turn the right face a half turn
r2 :: Cube -> Cube
r2 = r . r


-- todo: implement f, b, d and associated turns

l :: Cube -> Cube 
l = y2 . r . y2 

l' :: Cube -> Cube
l' = y2 . r' . y2

l2 :: Cube -> Cube
l2 = y2 . r2 . y2

u :: Cube -> Cube
u = z . r . z'

u' :: Cube -> Cube
u' = z . r' . z'

u2 :: Cube -> Cube
u2 = z . r2 . z'

-- Face rotation
clockwise :: Face -> Face
clockwise ( Face (s1,s2,s3,s4,s5,s6,s7,s8,s9) ) = Face (s7,s4,s1,s8,s5,s2,s9,s6,s3) 

anticlockwise :: Face -> Face
anticlockwise = inverse4 clockwise

--------------------
-- Cube rotations --
--------------------
--  i.e. changing the orientation of the cube without turning any face

-- Rotating like the u move
--  or Turning Face 4 to Face 3 
y :: Cube -> Cube
y ( Cube (f1, f2, f3, f4, f5, f6) ) = Cube ((clockwise f1), f3, f4, f5, f2, (anticlockwise f6))

y' :: Cube -> Cube
y' = inverse4 y

y2 :: Cube -> Cube
y2 = y . y

-- Rotating like the r move
--  or Turning Face 3 to Face 1
x :: Cube -> Cube
x ( Cube (f1, f2, f3, f4, f5, f6) ) = Cube (f3, (anticlockwise f2), f6, (clockwise f4), f1, f5)

x' :: Cube -> Cube
x' = inverse4 x

x2 :: Cube -> Cube
x2 = x . x

-- Rotating like the f move
--  or Turning Face 2 to Face 1
z :: Cube -> Cube
z ( Cube (f1, f2, f3, f4, f5, f6) ) = Cube ( f2, f6, (clockwise f3), f1, (anticlockwise f5), f4)

z' :: Cube -> Cube
z' = inverse4 z

z2 :: Cube -> Cube
z2 = z . z
