@title[Введение]

@snap[midpoint span-80]
## Низкоуровневая оптимизация программ на Haskell 
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
@ul[](false)
- 900 млн. вершин 17 млрд. рёбер
- Данные хранятся в памяти
- Данные неизменяемы
- Запрос затрагивает небольшое количество вершин
@ulend

Note:
- О себе: разработчик графовой платформы
- О графе (+ почему важно что запросы не раскрывают весь граф)
- Как представлять данные?

---

@title[Способы представления данных в памяти]

Способы представления данных в памяти

@ul[](false)
- Структуры данных
- Структуры массивов примитивных типов
- "Off-heap"
@ulend

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

@title[Работа с off-heap в Haskell]

@snap[north span=20]
Off-heap в Haskell
@snapend

@snap[west span=20]
Foreign.Storable
@snapend

@snap[south-west span-45 text-07]
@code[haskell](assets/src/Storable.hs)
@snapend

@snap[east span=20]
Бинарная сериализация
@snapend

@ul[south-east span-45 text-07](false)
- binary
- cereal
- protocol-buffers
- store
- persist
- flat
@ulend

Note:
- Бинарная сериализация
  - Данные фиксированных размеров
  - Не безопасна
  - Подходит для работы со структурами Си

---

@title[Интерфейс ридера]
<!-- пример на binary для тестов производительности -->
---
@title[Чтение вручную]
<!-- + asm дамп -->
---
@title[Сравнение binary и чтения вручную]
<!-- аллокация памяти во время чтения -->
---
@title[Особенности дизайна binary-подобных ридеров]
<!-- Библиотеки cereal, binary, protocol-buffers -->
<!-- lazy Bytestring -->
<!-- Codensity http://comonad.com/reader/2012/unnatural-transformations-and-quantifiers/ -->
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



<!-- 

---?color=linear-gradient(to right, #c02425, #f0cb35)
@title[Introduction]

<! --
Tip! Get started with this template as follows:
Step 1. Delete the contents of this PITCHME.md file.
Step 2. Start adding your own custom slide content.
Step 3. Copy slide markdown snippets from template/md directory as needed.
-- >

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
-->