-- Программы на Haskell являются коллекцией модулей.
-- Главный модуль должен называться Main.
-- Все имена модулей в Haskell называются с большой буквы.
-- Отдельный модуль - отдельный файл. Рекомендуется называть файл так же, как и модуль.

-- Как объявляется модуль:
module Demo where
-- В каждый модуль неявно импортируется модуль Prelude со стандартными функциями.

-- Как импортировать модуль:
import Data.Char -- модуль Char, лежит в директории Data

-- Как импортировать функции из модуля:
import Data.Char (toUpper, toLower)

-- Как импортировать все функции кроме некоторых:
import Data.Char hiding (toUpper, toLower)

-- КОНФЛИКТ ИМЕН
-- import Data.List
-- import Data.Set
-- в обоих модулях определена функция union,
-- поэтому придется использовать полное имя, например, Data.List.union

-- Чтобы разрешить, можно добавить qualified перед именем модуля:
-- тогда перед функциями модуля всегда придется прописывать полное имя.
import Data.List
import qualified Data.Set
-- теперь union - это Data.List.union
-- а union из Data.Set нужно вызывать как Data.Set.union

-- ПСЕВДОНИМЫ для модулей
import qualified Data.Set as Set
-- NB: если бы было без qualified, то к функциям можно было обращатьс и через Set, и просто по имени.
-- Т.е. пространство имен засорилось бы.

-- Как регулировать возможность импорта из модуля: см. файл Test.hs

-- КОМПИЛЯЦИЯ кода в Haskell
-- Шаги, которые выполняется Haskell,
-- чтобы получить двоичный код или байт-код для интерпретатора:
-- 1) Синтаксический разбор файла исходного кода.
-- Все имена делаются квалифицированными.
-- 2) Этап проверки типов.
-- 3) Этап "рассахаривания": код транслируется в более низкоуровневый язык Core
-- (тоже читаемый, но с более бедным синтаксисом - лишенный синтаксического сахара)
-- 4) Над полученной программой выполняется несколько циклов оптимизации.
-- (здесь код все еще человекочитаемый)
-- 5) Кодогенерация:
--  5.1) перевод код Core в код STG-машины (осуществляет графовую редукцию программы)
--  5.2) перевод в код C--
--  5.3) перевод в код целевой платформы/LLVM/...
--
-- P.S. если интереснее подробнее в этом разобраться, то можно почитать вот эту статью:
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/generated-code
