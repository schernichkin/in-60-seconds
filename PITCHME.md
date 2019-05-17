@title[Введение]

@snap[midpoint span-80]
### Низкоуровневая оптимизация программ на Haskell 
На примере библиотек двоичной десериализации
@snapend

@snap[south]
FPURE 2018
@snapend

Note:
- O себе: Сбербанк, Департамент управления данными, BigData.
- Содержание доклада: оптимизация программ, микробенчмарки, дамп кода, примеры техник низкоуровневой оптимизации
- Вступление к след сайту (почему бинарный ридер?)

---

@title[Область применения]

Графовая база данных  

- 900 млн. вершин 17 млрд. рёбер
- Данные хранятся в памяти
- Данные неизменяемы
- Запрос затрагивает небольшое количество вершин

Note:
- О себе: разработчик графовой платформы
- О графе (+ почему важно что запросы не раскрывают весь граф)
- Как представлять данные?

---

@title[Способы представления данных в памяти]

Способы представления данных в памяти

- Структуры данных
- Структуры массивов примитивных типов
- "Off-heap"

Note:
- Структуры данных
  - GC ведёт к обходу графа
    - Card marking (только для младших поколений)
    - GHC.Compact (копирование без разделения, compactWithSharing - в 10 раз медленнее)
    - Hакладные расходы (Нaskell - 1 слово на заголовок, JVM - 2 слова)
- Структуры массивов примитивных типов
  - Заимствованы из Struct of Arrays
  - Поля - только примитивные типы
  - Сложно представлять данные с переменной длинной
  - Требуется поддержка больших массивов (JVM)
- Off-heap
  - Произвольные структуры
  - Требуется эффективная бинарная сериализация
  - Прямая работа с памятью, небезопасно 

---

@title[neo4j]

@snap[north]
Конфигурация памяти neo4j
@snapend

@snap[midpoint span-100]
![neo4j](assets/img/neo4j-memory-management.svg)
@snapend

Note:
- neo4j - популярная графовая база данных, написанная на java

---

@title[Off-heap в Haskell]

Off-heap в Haskell

- Foreign.Storable
- Бинарная сериализация

---

@title[Foreign.Storable]

Foreign.Storable

```haskell
class Storable a where
    sizeOf      :: a -> Int
    alignment   :: a -> Int
    peekElemOff :: Ptr a -> Int      -> IO a
    pokeElemOff :: Ptr a -> Int -> a -> IO ()
    peekByteOff :: Ptr b -> Int      -> IO a
    pokeByteOff :: Ptr b -> Int -> a -> IO ()
    peek        :: Ptr a      -> IO a
    poke        :: Ptr a -> a -> IO ()
```
Note:
- Данные фиксированных размеров
- Не безопасна
- Подходит для работы со структурами Си

---

@title[Бинарная сериализация]

Бинарная сериализация

```haskell
getPerson :: Get Person
getPerson = do 
    age     <- fromIntegral <$> getWord8
    nameLen <- fromIntegral <$> getWord8
    name    <- decodeUtf8   <$> getByteString nameLen
    return $ Person name age

readPerson :: ByteString -> Person
readPerson = runGet getPerson  
```

Note:
- Библиотека - binary
- Десериализатор модульный, состоит из более простых ридеров
- Монадический интерфейс необходим для чтения данных произвольной длины

---

@title[Disclaimer]

Любая оптимизация начинается с профилирования

Note:
- Жалоба на медленную работу в определенном сценарии использования
- Профилирование сценария использования
- Оптимизация алгоритма
- Низкоуровневая оптимизация

---

@title[Базовая оценка производительности]

Базовая оценка производительности:<br/>
12 * Word64 + Word32 (100 байт)
@code[haskell text-08](assets/src/hw/read12Int64PlusInt32.hs)

@[3]
@[4]
@[5]
@[6]
@[7]
@[8]
@[10]
@[11-12]
@[13]
@[1-13]

Note:
- Считываем 100 байт (12 Word64 и 1 Word32) и суммируем их.

--- 

@title[Базовая оценка производительности - asm dump]

-ddump-asm

@code[x86asm text-07](assets/src/hw/read12Int64PlusInt32.asm)
@[1-5] 
@[6-8] 
@[10-13] 
@[21-25]
@[15, 17-18, 20, 27, 29]
@[0-29]

---

@title[Binary - код]

binary
@code[haskell text-08](assets/src/binary/read12Int64PlusInt32.hs)

Note: 
- не нужно указывать смещения
- код безопасен
- код модулярен

---

@title[Binary - результаты]

criterion

@code[txt text-06](assets/src/read12Int64HB.sh)

@code[txt text-07](assets/src/read12Int64HB.txt)
@[1-2,13-14]
@[1, 9-10, 13, 21-22]
@[1-25]

---

@title[Дизайн binary]
binary
@[1, 3-6]
@[8]
@[10-13]
@[15-21]
@[1-21]

@code[haskell text-08](assets/src/binary/get.hs)

---

Note:
When ghc compiles your program, after lexical analysis and syntax analysis it first removes syntactical sugar and does scope analysis; then, after type checking, it translates your code to core, as we saw. The core gets optimized and then translated to stg (for “Spineless Tagless G-machine”); stg code is very similar to core, but with a handful of additional restrictions; most importantly, all constructor applications must be fully applied (and if not, an explicit lambda must be constructed). Finally, the stg code gets translated to C--, which is a portable assembly language similar in intent to llvm, and the C-- code then is translated to assembly language.

<!-- Библиотеки cereal, binary, protocol-buffers -->
<!-- lazy Bytestring -->
<!-- Codensity http://comonad.com/reader/2012/unnatural-transformations-and-quantifiers/ -->

---

@ul[south-east span-45 text-07](false)
- binary
- cereal
- protocol-buffers
- store
- persist
- flat
@ulend

---
@title[Особенности дизайна store]
<!-- замер производительности store -->
---
@title[Дамп кода store]
---
@title[Встраивание (Inlining)]
<!-- Упомянуть, при каких условиях может произойти встраивание. -->
---
@title[Дизайн lev-tolstoy]
<!-- назад к сodensity -->
<!-- статическая часть -->
<!-- indexed monad для вычисления смешений -->
---
@title[Дамп кода lev-tolstoy]
<!-- rebindable syntax -->
<!-- дамп кода статической части -->
---
@title[Сравнение производительности lev-tolstoy и store]





---?color=linear-gradient(to right, #c02425, #f0cb35)
@title[Introduction]

<!--
Tip! Get started with this template as follows:
Step 1. Delete the contents of this PITCHME.md file.
Step 2. Start adding your own custom slide content.
Step 3. Copy slide markdown snippets from template/md directory as needed.
-->

@snap[west text-25 text-bold text-white]
GitPitch<br>*The Template*
@snapend

@snap[south-west byline text-white text-06]
The Fastest Way From Idea To Presentation.
@snapend

---
@title[Slide Markdown]

### Each slide in this presentation is provided as a *template*.

<br><br>

@snap[south span-100 text-purple text-05]
Reuse the *markdown snippet* for any slide in this template within your own @css[text-gold text-bold](PITCHME.md) files.
@snapend

---
@title[Tip! Fullscreen]

![TIP](template/img/tip.png)
<br>
For the best viewing experience, press F for fullscreen.
@css[template-note](We recommend using the *SPACE* key to navigate between slides.)

---?include=template/md/split-screen/PITCHME.md

---?include=template/md/sidebar/PITCHME.md

---?include=template/md/list-content/PITCHME.md

---?include=template/md/boxed-text/PITCHME.md

---?include=template/md/image/PITCHME.md

---?include=template/md/sidebox/PITCHME.md

---?include=template/md/code-presenting/PITCHME.md

---?include=template/md/header-footer/PITCHME.md

---?include=template/md/quotation/PITCHME.md

---?include=template/md/announcement/PITCHME.md

---?include=template/md/about/PITCHME.md

---?include=template/md/wrap-up/PITCHME.md

---?image=template/img/presenter.jpg
@title[The Template Docs]

@snap[west sign-off]
### Now it's your turn.
@snapend

@snap[south docslink text-gold span-100]
For supporting documentation see the [The Template Docs](https://gitpitch.com/docs/the-template)
@snapend
