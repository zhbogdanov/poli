{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.List.Split
import Data.String
import Data.Sort
import Data.List

-- Секция парсера входной сроки для получения внутреннего представления данных

-- После сплита на слагаемые по '-' добавляем ко второму слагаемому минус
useOtr::[[String]]->[[String]]
useOtr [] = []
useOtr (x:xs) = do
    if length x == 2 then [[x !! 0, "-" ++ x !! 1]] ++ useOtr(xs)
    else [x]  ++ useOtr(xs)

-- Конкатенация списков строк (стандартная не хочет работать)
concatList::[[String]]->[String]
concatList [] = []
concatList (x:xs) = x ++ concatList(xs)

-- При сплите пустое значение перед X означает, что стоит 1
emptyToOne::String->String
emptyToOne str = do
    if str == "" then "1"
    else str

-- Дергаем коэффициент и степень в слагаемом со знаком степени
splitPow::String->(Int, Int)
splitPow str = (read (emptyToOne (split !! 0)) :: Int, read (emptyToOne (split !! 1)) :: Int)
    where split = splitOn "x^" str

-- Дергаем коэффициент и степень в слагаемом
splitFirstPow::String->(Int, Int)
splitFirstPow str = (read (emptyToOne (split !! 0)) :: Int, 1)
    where split = splitOn "x" str

-- Из строки слагаемого получает кортеж вида (коэффициент, степень)
stringToTuple::String->(Int, Int)
stringToTuple str = do
    if (elem 'x' str) && (elem '^' str) then splitPow(str)
    else if (elem 'x' str) then splitFirstPow(str)
    else (read str :: Int, 0)

-- Из всех слагаемых получаем кортежи вида (коэффициент, степень)
createTuples::[String]->[(Int, Int)]
createTuples [] = []
createTuples (x:xs) = [stringToTuple(x)] ++ createTuples(xs)

-- Фильтруем слагаемые с коэффициентом ноль и применяем вышеперечисленные функции
parser::String->[(Int, Int)]
parser str = filter (\el -> not ((fst el) == 0)) (createTuples (filter (\el -> el /= "") (concatList (useOtr(map (\el -> splitOn "-" el) (splitOn "+" str))))))


-- Секция перемножения кортежей


-- Декртово произведение
decartProd::[(Int, Int)]->[(Int, Int)]->[(Int, Int)]
decartProd xs ys = [(fst x * fst y, snd x + snd y) | x <- xs, y <- ys]

-- Сравнение кортежей на равные степени
powEq::(Int, Int)->(Int, Int)->Bool
powEq (_, pow1) (_, pow2) = pow1 == pow2

-- Группировка по степеням и суммирование коэффициентов
sumGroup::[(Int, Int)]->(Int, Int)
sumGroup (x:xs) = (sum (map fst (x:xs)), snd x)

-- Применение группировки ко всем кортежам
sumThem::[(Int, Int)]->[(Int, Int)]
sumThem list = map sumGroup (groupBy powEq (sortOn snd list))


-- Секция перевода кортежей в строгу ответа


-- Перевод кортежа в строку
tupleToString::(Int, Int)->String
tupleToString x = do
    if (snd x == 0) then show (fst x)
    else if (snd x == 1) then show (fst x) ++ "x"
    else show (fst x) ++ "x^" ++ show (snd x)

-- Перевод всех кортежей в строки
createString::[(Int, Int)]->[String]
createString [] = []
createString (x:xs) = [tupleToString x] ++ createString xs

-- Добавление знаков между слагаемыми
transformIntoPoli::Bool->[String]->String
transformIntoPoli _ [] = ""
transformIntoPoli firstFlag (x:xs) = do
    if (x !! 0) == '-' || firstFlag then x ++ (transformIntoPoli False xs)
    else "+" ++ x ++ (transformIntoPoli False xs)


main :: IO ()
main = do
    putStrLn "Enter first polynomial"
    poli1 <- getLine
    putStrLn "Enter second polynomial"
    poli2 <- getLine
    putStr "Результат перемножения полиномов: "
    putStrLn (transformIntoPoli True (createString(sumThem(decartProd (parser poli1) (parser poli2)))))

-- Tests

--1) -8x^3+x-1+10x^2
--2) -8x^3+x-1+10x^2
--Result: 1-2x-19x^2+36x^3+84x^4-160x^5+64x^6

--1) -8x^3+x-1+10x^2
--2)
--Result:

--1)
--2) -8x^3+x-1+10x^2
--Result:

--1) 1+0x
--2) -8x^3+x-1+10x^2
--Result:-1+1x+10x^2-8x^3

--1) 0x
--2) -8x^3+x-1+10x^2
--Result:
