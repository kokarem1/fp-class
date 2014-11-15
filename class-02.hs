-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms secs
  | secs >= 0 = (hours, minutes, seconds)
  | otherwise = error "secs must be >= 0" 
  where
    hours = secs `div` 3600
    minutes = (secs `div` 60) - (60 * hours)
    seconds = secs - (hours * 3600) - (minutes * 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s)
  | h >= 0 && m >= 0 && s >= 0 = h * 3600 + m * 60 + s
  | otherwise = error "h, m, s must be >= 0"

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s) 

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ( (y2 - y1) ^ 2 + (x2 - x1) ^ 2 )

triangle :: Point -> Point -> Point -> (Double, Double)
triangle p1 p2 p3
  | not (b + c > a && a + c > b && a + b > c) = error "such triangle don't exists" 
  | otherwise = (p, s)
  where
    a = distance p1 p2
    b = distance p2 p3
    c = distance p1 p3
    p = a + b + c
    s = sqrt ( p * (p - a) * (p - b) * (p - c) )

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
  | x `mod` 2 == 0 = 1 + nEven xs
  | otherwise = nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
twice :: Num x => x -> x
twice x = 2 * x

doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = twice x : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
  | odd x = x : fltOdd xs
  | otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).

neg :: (Num x, Ord x) => x -> Bool
neg x = if x < 0 then True else False

-- a)
delNeg :: (Num a, Ord a) => [a] -> [a]
delNeg [] = []
delNeg (x:xs)
  | not (neg x) = x : delNeg xs
  | otherwise = delNeg xs
  
-- б)
doubleEvenElems :: Integral a => [a] -> [a]
doubleEvenElems [] = []
doubleEvenElems (x:xs)
  | even x = twice x : doubleEvenElems xs
  | otherwise = x : doubleEvenElems xs

-- в)
swapOddEvenPos :: [a] -> [a]
swapOddEvenPos [] = []
swapOddEvenPos (x1:x2:xs) = x2 : x1 : swapOddEvenPos xs
swapOddEvenPos (x:xs) = swapOddEvenPos xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x + y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine :: [a] -> [b] -> [(a,b)]
combine [] ys = []
combine xs [] = []
combine (x:xs) (y:ys) = (x,y) : combine xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
-- б) в порядке возрастания.

-- а)
getNNatDesc :: Integer -> [Integer]
getNNatDesc 0 = []
getNNatDesc n = n : getNNatDesc (n - 1)

-- б)
getNNat :: Integer -> [Integer]
getNNat n = getNNatIter 1
  where
    getNNatIter i
      | i /= n = i : getNNatIter (i + 1)
      | otherwise = [n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
-- б) Eq a => [a] -> a -> Bool
-- в) [a] -> Int -> [a]
-- г) a -> Int -> [a]
-- д) [a] -> [a] -> [a]
-- е) Eq a => [a] -> [[a]]
-- ж) [a] -> [(Int, a)]
-- з) Eq a => [a] -> [a]
