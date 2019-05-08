@title[Введение]

@snap[midpoint span-80 text-05]
## Низкоуровневая оптимизация программ на Haskell 
На примере двоичной десериализации
@snapend

@snap[south text-purple text-05]
FPURE 2018
@snapend

---

@title[Область применения]

@snap[north text-08 span-80]
Почему двоичная десериализация?
@snapend

@snap[midpoint text-08 span-80]

Пример использования: Графовая база данных  
<br />
@ul[](false)
- &lt;кол-во&gt; вершин &lt;кол-во&gt; рёбер
- Запрос затрагивает небольшое количество вершин
- Данные неизменяемы
@ulend

@snapend

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