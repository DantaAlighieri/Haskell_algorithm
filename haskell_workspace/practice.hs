[1,2] ++ [3,4]
1:[2,3,4]

take 3 [1,2,3,4]
drop 3 [1,2,3,4]
head [1,2,3,4]
tail [1,2,3,4]
init [1,2,3,4,5]
last [1,2,3,4,5]
elem 1 [1,2,3,4,5]
[1,2,3,4,5] !! 2
1 `elem` [1,2,3,4]
[1,3..5]
[1,3..11]

列表推到式
[X *2 | <- [1..10], X * 2 >= 12]

[X*2 | X <- [1..10], X `MOD` 2 == 1]

[1..] 1 到无穷大

take 10 [x | <- [1..], x `mod` 7 == 0 || x `mod` 10 == 7]

"hello world"
"hello world" !! 2
take 5 " hello world

triple x = 3 * x

distance x y = sqart ( x * x + y * y)
distance 2 3
describe "ccg" = "tql"
:{
describe "zd" = "more moey than god"
describe "lfs" = "vegetable exploded"
describe x = "no info"
:}
describe "ccg"
有点类似于map，得到tql, :{多行输入}
:{
f x
 | x <= 10 x
 | x <= 20 = x/2
 | x <= 30 = x * x
 otherwise = x * x * x
:}
列表的模式匹配，列表的map？？
:{
listFunction [] = "empty"
listFunction (x : []) = "only one!"
listFunction ([x]) = "only one!"
这两个一样

listFunction (x:y[]) = "two"
listFunction (x:y_) = "more than two"
_代表还有更多的东西

listFunction [1] = only one
listFunction [1,2] two
listFunction [] empty
listFunction [1,2,3,4] = more than two

:}

递归


:{
maxInList [x] = x
maxIntList (x:xs) = max x (maxInList xs)
x：xs把这个数组拆成2部分，第一部分是头部元素，第二部分是后面的链表
:}

反转列表
:{
reverse [] = []
reverse [x] = [x]
reverse (x : xs) = reverse xs ++ [x]

:}

快速排序
:{
quicksort [] = []
quicksort (x:xs) = (quicksort [y | y <- xs, y < x]) ++  [x] ++
(quicksort [y | y <- xs, y >= x])
:}

distance 2 3

length :: floating a => (a -> (a -> (a)))

创建偏函数
f = distance 2
f 3
==   distance 2 3
(*3)
(<10)

高阶函数
map(*3) [1,2,3,4,5]
[3,6,9,12,15]

filter (<10) [3,6,9,12,15]

foldl (\acc x -> acc + x) 0 [3, 6, 9]

foldl (+) 0 [3, 6, 9]

class
Eq Ord

functor函子
fmap (*3) [1,2,4]
[3,6,12]
fmap 其实相当于map

maybe要么有值要么nothing

Just
Nothing
instance Functor Maybe where
    fmap func (Just x) = Just (func x)
    fmap func Nothing = Nothing

Just (*3) <*> (Just 10)
--Just 30

:{
half x = if even x
    then Just (x `div` 2)
    else Nothing

:}

Just 20>>= half

Just 20 >>= half
--Just 10
Just 20 >>= half >>= half
--Just 5
Just 20>>= half >>= half >>= half
Nothing

把一个数字放到函数管道中一直计算下去

MVVM
领域层 ---VM层
render函数---由MVVM框架负责提供
用户行为---是指修改该了VM层的数据

Flux
Redux
单一的store

