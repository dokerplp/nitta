# nitta
NITTA - реконфигурируемая вычислительная платфома.

Статус проекта: ранний прототип.

Отличительные особенности:
1. Реконфигурируемость (алгоритма синтеза, аппаратных компонент и микроархитектуры).
2. Возможность работы в связке со специализируемым программируемым процессором, что позволяет 
   менять код не меняя прошивку для ПЛИС.

## Рекомендуемый инструментарий:
1. Для работы с Haskell-ом:
  - система сборки - https://docs.haskellstack.org/en/stable/README/
      - Для установки на Windows используйте установочный пакет.
      - После установки зайдите в каталог проекта nitta и установите рабочее окружение командой `stack setup` и сделайте первую сборку `stack build`.
  - среда разработки - https://code.visualstudio.com с расширениями:
      - Haskero (требует установить `intero`, для этого в консоли из папки проекта выполните `stack install intero`);
      - stylish-haskell (требует установить `stylish-haskell`, для этого в консоли из папки проекта выполните `stack install stylish-haskell`); 

2. Для Verilog-а:
  - симулятор (используется при автоматическом тестировании вычислительных блоков) - 
    http://iverilog.icarus.com
  - просмотр временных диаграмм: http://gtkwave.sourceforge.net

У проекта есть две точки входа, обе в той или иной степени кривые:
1. app/Main.hs - в настоящий момент используется для ручного интеграционного тестирования.
2. test/Spec.hs - используется для автоматизированного тестирования.

## Порядок включения в работу:
1. Делаете fork проекта. Для этого:
  1. Заходите на страницу проекта: http://nitta.io/aleksanr.penskoi/nitta
  2. Нажимаете кнопку `Fork` сверху-справа.
  3. Стандартные настройки должны вам подойти, по этому просто нажимайте `создать`.

2. Устанавливаем софт и настраиваем авторизацию по ключу (инструкция для Windows).
  1. Используется клиент SouceTree + консоль. Вам понадобится аккаунт на `bitbucket.org` 
     и установить приложение: https://www.sourcetreeapp.com/. Рекомендую сразу перевести 
     интерфейс на английский, так как: одна терминология, не будет писать русским в commit messages.
  2. В приложении открываем меню `Действия`, жмем на `Открыть в терминале`.
  3. В открывшемся терминале вводим следующее: `ssh-keygen.exe` (жмем Enter во всех случаях).
     Это приведёт к генерации пары из публичного и приватного ключа. 
  4. В терминале выполните команду `cat .ssh/id_rsa.pub`. Она выведет в терминал ваш публичный ключ.
     Его необходимо скопировать в буфер обмена.
  5. Зарегестрируйте ваш публичный ключ в репозитории. Для этого откройте `Your Settings" 
     на сайте `nitta.io`, раздел `SSH Keys`. Нажмите кнопку `Add Key`. В поле `Content` 
     вставьте публичный ключ. Сохраните.
  6. Далее в терминале вводим команду `git clone <ssh адрес репозитория>`.
  7. После копирования перетащите скопированный репозиторий в окно Sourcetree, предварительно 
     щелкнув на вкладку Local в окне программы.
  8. Заходим в настройки SourceTree (`Tools` -> `Options`). На вкладке `General`, раздел 
     `SSH Client Configuration`, пункт `SSH Client` - выберите `OpenSSH`.

## Направления работ / задачи:
- Средства прикладного программирования
  - IEC 61131 FB
  - Arrows
  - C
  - vinsim
  - Процедуры
- Пользовательский интерфейс
  - Представление многоуровневого вычислительного процесса (фильтрация, свёртки)
  - Представление процесса в ветвящемся времени
  - Представление данных функционального моделирования
  - Представление данных симуляции целевой системы
  - Представление потока управления и потока данных
  - Представление модели вычислителя
  - Управление процессом компиляции
- Алгоритмы синтеза
  - Генерация модели процессора
  - Вложенные шины
  - Иные коммуникационыне среды
  - Конвейра
  - Оптимизация
  - API для внешнего управления
- Верификация компонент САПР
- Вычислительные блоки
  - Автоматизация процесса поддержки
  - Новые вычислительные блоки
- Разное:
  - Демонстрационный стенд.
  - Типизация передаваемых данных.
