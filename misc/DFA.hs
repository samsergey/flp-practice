data FSMachine s i =
  FSMachine { alphabet :: [i] -- допустимые символы
            , delta :: s -> i -> s -- функция перехода
            , start :: s -- начальное состояние
            , stop :: [s] -- останавливающие состояния
            , final :: [s] -- конечные состояния
 }

mod3 :: FSMachine Int Int 
mod3 = FSMachine [0,1] f 0 [] [0,1,2]
  where f 0 0 = 0
        f 0 1 = 1
        f 1 0 = 2
        f 1 1 = 0
        f 2 0 = 1
        f 2 1 = 2

scanFSM :: (Eq s, Eq i)
  => FSMachine s i -> [i] -> [(s, i)]
scanFSM m xs = takeWhile (not . halt . fst) 
               $ zip states inputs
  where
    -- поток допустимых символов
    inputs = takeWhile (`elem` alphabet m) xs
     -- поток состояний автомата
    states = tail $ scanl (delta m) (start m) inputs
     -- условие остановки работы
    halt = (`elem` stop m)

abba = FSMachine "ab" f 0 [-1] [4]
  where f s x = case (s, x) of
          (0, 'a') -> 1
          (0, 'b') -> -1
          (1, 'a') -> -1
          (1, 'b') -> 2
          (2, 'a') -> 4
          (2, 'b') -> 2
          (4, _) -> 4

runFSM :: (Eq s, Eq i) => FSMachine s i -> [i] -> s
runFSM m xs = case scanFSM m xs of
                [] -> start m
                res -> fst $ last res

testFSM :: (Eq s, Eq i) => FSMachine s i -> [i] -> Bool
testFSM m = (`elem` final m) . runFSM m
