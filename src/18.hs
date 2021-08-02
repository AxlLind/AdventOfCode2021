import Aoc
import Data.Function
import Text.Read
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

input = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 952\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19"

data Src = Reg Char | Val Int

data Op = Snd Src
        | Set Char Src
        | Add Char Src
        | Mul Char Src
        | Mod Char Src
        | Rcv Char
        | Jgz Src Src

parseInput :: String -> Vector Op
parseInput s = lines s & map (parseOp . words) & Vec.fromList
  where
    parseSrc s = case readMaybe s of
      Just n -> Val n
      Nothing -> Reg (head s)
    parseOp ["snd",x]   = Snd (parseSrc x)
    parseOp ["set",x,y] = Set (head x) (parseSrc y)
    parseOp ["add",x,y] = Add (head x) (parseSrc y)
    parseOp ["mul",x,y] = Mul (head x) (parseSrc y)
    parseOp ["mod",x,y] = Mod (head x) (parseSrc y)
    parseOp ["rcv",x]   = Rcv (head x)
    parseOp ["jgz",x,y] = Jgz (parseSrc x) (parseSrc y)
    parseOp _ = error "invalid input"

fetchSrc :: Map Char Int -> Src -> Int
fetchSrc _ (Val v) = v
fetchSrc regs (Reg c) = Map.findWithDefault 0 c regs

part1 :: Vector Op -> Int -> ([Int], [Int], Map Char Int) -> Int
part1 ops ip state = if recv /= [] then head recv else part1 ops (ip + offset) newState
  where
    execOp (sent, recv, regs) (Snd x)   = (1, (fetchSrc regs x:sent, recv, regs))
    execOp (sent, recv, regs) (Set x y) = (1, (sent, recv, Map.insert x (fetchSrc regs y) regs))
    execOp (sent, recv, regs) (Add x y) = (1, (sent, recv, Map.insert x (fetchSrc regs (Reg x) + fetchSrc regs y) regs))
    execOp (sent, recv, regs) (Mul x y) = (1, (sent, recv, Map.insert x (fetchSrc regs (Reg x) * fetchSrc regs y) regs))
    execOp (sent, recv, regs) (Mod x y) = (1, (sent, recv, Map.insert x (fetchSrc regs (Reg x) `mod` fetchSrc regs y) regs))
    execOp (sent, recv, regs) (Rcv x)   = (1, (sent, if fetchSrc regs (Reg x) == 0 then recv else head sent:recv, regs))
    execOp (sent, recv, regs) (Jgz x y) = (if fetchSrc regs x > 0 then fetchSrc regs y else 1, (sent, recv, regs))

    (offset, newState) = execOp state (ops!ip)
    (_,recv,_) = newState


part2 :: Vector Op -> (Int, Int, [Int], [Int], Map Char Int, Map Char Int) -> Int
part2 ops (ip0,ip1,q0,q1,regs0,regs1) = if (offset0,offset1) == (0,0) then 0 else length sent1 + part2 ops newState
  where
    execOp regs queue     (Snd x)   = (1, [fetchSrc regs x], queue, regs)
    execOp regs queue     (Set x y) = (1, [], queue, Map.insert x (fetchSrc regs y) regs)
    execOp regs queue     (Add x y) = (1, [], queue, Map.insert x (fetchSrc regs (Reg x) + fetchSrc regs y) regs)
    execOp regs queue     (Mul x y) = (1, [], queue, Map.insert x (fetchSrc regs (Reg x) * fetchSrc regs y) regs)
    execOp regs queue     (Mod x y) = (1, [], queue, Map.insert x (fetchSrc regs (Reg x) `mod` fetchSrc regs y) regs)
    execOp regs (q:queue) (Rcv x)   = (1, [], queue, Map.insert x q regs)
    execOp regs []        (Rcv _)   = (0, [], [], regs)
    execOp regs queue     (Jgz x y) = (if fetchSrc regs x > 0 then fetchSrc regs y else 1, [], queue, regs)

    (offset0, sent0, newQ0, newRegs0) = execOp regs0 q0 (ops!ip0)
    (offset1, sent1, newQ1, newRegs1) = execOp regs1 q1 (ops!ip1)
    newState = (ip0+offset0, ip1+offset1, newQ0++sent1, newQ1++sent0, newRegs0, newRegs1)

solveParts :: Int -> (Int,Int)
solveParts _ = (p1,p2)
  where
    ops = parseInput input
    p1 = part1 ops 0 ([], [], Map.empty)
    p2 = part2 ops (0,0,[],[], Map.fromList [('p',0)], Map.fromList [('p',1)])

main = do
  Aoc.timer solveParts
