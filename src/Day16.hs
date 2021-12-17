{-# LANGUAGE ViewPatterns #-}
module Day16
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Version = Int
type Id = Int

data Packet = Lit Version Int | Op Version Id [Packet] | Empty deriving (Eq, Show)

type Input = [Packet]

solution :: Solution
solution = (part1, part2, "day16")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: [Packet] -> Int
processPart1 = foldl addVal 0
  where
  addVal acc pk = acc + value 0 [pk]
  value acc []                = acc
  value acc ((Lit v _):xs)    = value (acc + v) xs
  value acc ((Op v _ sub):xs) = value (acc + v) (xs ++ sub)
  value acc (Empty:xs)        = value acc xs

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: [Packet] -> Int
processPart2 = foldl addVal 0
  where
  addVal acc pk = acc + value pk
  value ((Lit _ l ))           = l
  value ((Op v 0 sub))         = sum $ value <$> sub
  value ((Op v 1 sub))         = product $ value <$> sub
  value ((Op v 2 sub))         = minimum $ value <$> sub
  value ((Op v 3 sub))         = maximum $ value <$> sub
  value ((Op v 5 (fst:snd:_))) = if value fst > value snd then 1 else 0
  value ((Op v 6 (fst:snd:_))) = if value fst < value snd then 1 else 0
  value ((Op v 7 (fst:snd:_))) = if value fst == value snd then 1 else 0
  value _                      = 0

parseInput :: String -> Input
parseInput raw = decodePackets [] (concat [decodeHex hex i | (i, hex) <- zip [1..] raw, hex `elem` "0123456789ABCDEF"])
    where
    decodePackets acc [] = filter (/= Empty) acc
    decodePackets acc s = decodePackets (acc ++ [packet]) rest
        where
        (packet, rest') = decodePacket s
        rest = withPadding rest'

decodePacket :: String -> (Packet, String)
decodePacket [] = (Empty, [])
decodePacket (extractHeader -> (version, 4, rest )) = extractLiteral version rest
decodePacket (extractHeader -> (version, id, rest )) = extractOperation version id rest

withPadding :: String -> String
withPadding s = drop (length s `mod` 4) s

extractHeader :: String -> (Version, Id, String)
extractHeader (splitAt 6 -> (header, rest)) = (toDec v, toDec i, rest)
  where
  (v, i) = splitAt 3 header

extractLiteral :: Version -> String -> (Packet, String)
extractLiteral v s = (Lit v (toDec value), rest)
   where
   extractSegment :: String -> (Char, String, String)
   extractSegment (splitAt 5 -> (i:raw, r)) = (i, raw, r)
   extractSegment _                         = undefined
   (i, raw, r) = extractSegment s
   (value, rest) = loop [] i raw r
   loop acc '0' raw rest = (acc ++ raw, rest)
   loop acc _ raw rest = loop (acc ++ raw) i' raw' r'
     where
     (i', raw', r') = extractSegment rest

extractOperation :: Version -> Id -> String -> (Packet, String)
extractOperation v id (lid:s) = (Op v id subpackets, rest)
  where
  (subpackets, rest) = parseSubpackets lid s
  parseSubpackets '0' s = (pks, rest')
    where
    (bits, s') = splitAt 15 s
    (raw, rest') = splitAt (toDec bits) s'
    pks = decodePackets [] raw
    decodePackets acc [] = filter (/= Empty) acc
    decodePackets acc s'' = decodePackets (acc ++ [packet]) rest
      where
      (packet, rest) = decodePacket s''
  parseSubpackets _ s   = (pks, rest')
    where
    (lpks, s') = splitAt 11 s
    (pks, rest') = foldl update ([], s') [1..(toDec lpks)]
    update (pks, rest'') _ = (pks ++ [pk], rest''')
      where
      (pk, rest''') = decodePacket rest''
extractOperation _ _ _ = (Empty, "")

decodeHex :: Char -> Int -> String
decodeHex '0' _ = "0000"
decodeHex '1' _ = "0001"
decodeHex '2' _ = "0010"
decodeHex '3' _ = "0011"
decodeHex '4' _ = "0100"
decodeHex '5' _ = "0101"
decodeHex '6' _ = "0110"
decodeHex '7' _ = "0111"
decodeHex '8' _ = "1000"
decodeHex '9' _ = "1001"
decodeHex 'A' _ = "1010"
decodeHex 'B' _ = "1011"
decodeHex 'C' _ = "1100"
decodeHex 'D' _ = "1101"
decodeHex 'E' _ = "1110"
decodeHex 'F' _ = "1111"
decodeHex x i   = error ("invalid hex on position: " ++ show i)
