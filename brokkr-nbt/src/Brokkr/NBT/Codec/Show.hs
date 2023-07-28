{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Brokkr.NBT.Codec.Show (
  showsPrecCodec 
) where

import Brokkr.NBT.Codec.Internal

import Brokkr.NBT.NBTString

showsPrecCodec :: forall ctx0 i0 o0 . Int -> NBTCodec ctx0 i0 o0 -> String -> String
showsPrecCodec _ TagCodec = showString "TagCodec"

showsPrecCodec prec (ByteCodec (Just name)) = showParen (prec > 10) $
    showString "ByteCodec "
  . showsPrec 11 name
showsPrecCodec _ (ByteCodec Nothing) = showString "ByteCodec"
showsPrecCodec prec (ShortCodec (Just name)) = showParen (prec > 10) $
    showString "ShortCodec "
  . showsPrec 11 name
showsPrecCodec _ (ShortCodec Nothing) = showString "ShortCodec"
showsPrecCodec prec (IntCodec (Just name)) = showParen (prec > 10) $
    showString "IntCodec "
  . showsPrec 11 name
showsPrecCodec _ (IntCodec Nothing) = showString "IntCodec"
showsPrecCodec prec (LongCodec (Just name)) = showParen (prec > 10) $
    showString "LongCodec "
  . showsPrec 11 name
showsPrecCodec _ (LongCodec Nothing) = showString "LongCodec"
showsPrecCodec prec (FloatCodec (Just name)) = showParen (prec > 10) $
    showString "FloatCodec "
  . showsPrec 11 name
showsPrecCodec _ (FloatCodec Nothing) = showString "FloatCodec"
showsPrecCodec prec (DoubleCodec (Just name)) = showParen (prec > 10) $
    showString "DoubleCodec "
  . showsPrec 11 name
showsPrecCodec _ (DoubleCodec Nothing) = showString "DoubleCodec"
showsPrecCodec prec (ByteArrayCodec (Just name)) = showParen (prec > 10) $
    showString "ByteArrayCodec "
  . showsPrec 11 name
showsPrecCodec _ (ByteArrayCodec Nothing) = showString "ByteArrayCodec"
showsPrecCodec prec (IntArrayCodec (Just name)) = showParen (prec > 10) $
    showString "IntArrayCodec "
  . showsPrec 11 name
showsPrecCodec _ (IntArrayCodec Nothing) = showString "IntArrayCodec"
showsPrecCodec prec (LongArrayCodec (Just name)) = showParen (prec > 10) $
    showString "LongArrayCodec "
  . showsPrec 11 name
showsPrecCodec _ (LongArrayCodec Nothing) = showString "LongArrayCodec"
showsPrecCodec prec (StringCodec (Just name)) = showParen (prec > 10) $
    showString "StringCodec "
  . showsPrec 11 name
showsPrecCodec _ (StringCodec Nothing) = showString "StringCodec"

showsPrecCodec prec (ListCodec (Just name) inner) = showParen (prec > 10) $
    showString "ListCodec "
  . showsPrec 11 name
  . showString " "
  . showsPrecCodec 11 inner
showsPrecCodec prec (ListCodec Nothing inner) = showParen (prec > 10) $
    showString "ListCodec "
  . showsPrecCodec 11 inner

showsPrecCodec prec (CompoundCodec (Just name) inner) = showParen (prec > 10) $
    showString "CompoundCodec "
  . showsPrec 11 name
  . showString " "
  . showsPrecCodec 11 inner
showsPrecCodec prec (CompoundCodec Nothing inner) = showParen (prec > 10) $
    showString "CompoundCodec "
  . showsPrecCodec 11 inner

showsPrecCodec prec (RequiredKeyCodec key inner (Just name)) = showParen (prec > 10) $
    showString "RequiredKeyCodec "
  . showsPrec 11 (toText key)
  . showString " "
  . showsPrec 11 name
  . showString " "
  . showsPrecCodec 11 inner
showsPrecCodec prec (RequiredKeyCodec key inner Nothing) = showParen (prec > 10) $
    showString "RequiredKeyCodec "
  . showsPrec 11 (toText key)
  . showString " "
  . showsPrecCodec 11 inner

showsPrecCodec prec (OptionalKeyCodec key inner (Just name)) = showParen (prec > 10) $
    showString "OptionalKeyCodec "
  . showsPrec 11 (toText key)
  . showString " "
  . showsPrec 11 name
  . showString " "
  . showsPrecCodec 11 inner
showsPrecCodec prec (OptionalKeyCodec key inner Nothing) = showParen (prec > 10) $
    showString "OptionalKeyCodec "
  . showsPrec 11 (toText key)
  . showString " "
  . showsPrecCodec 11 inner

showsPrecCodec prec (LmapCodec _ inner) = showParen (prec > 10) $
    showString "LmapCodec <<f>> "
  . showsPrecCodec 11 inner
showsPrecCodec prec (RmapCodec _ inner) = showParen (prec > 4) $
    showString "<<f>> <$#> "
  . showsPrecCodec 5 inner

showsPrecCodec _ (PureCodec _) = showString "Pure <<a>>"

showsPrecCodec prec (ApCodec ff fa) = showParen (prec > 4) $
    showsPrecCodec 5 ff
  . showString " <*#> "
  . showsPrecCodec 5 fa
